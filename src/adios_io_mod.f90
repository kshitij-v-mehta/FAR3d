MODULE adios_io_mod

  ! Thin, named-variable wrapper around the ADIOS2 Fortran API used by the
  ! restart-dump I/O in output_mod.f90 (wrdump/rddump/rddump_flds).
  !
  ! One adios2_adios instance is kept, bound to MPI_COMM_SELF (see adios_init):
  ! writes are already rank-0-only (wrdump is only ever called from behind an
  ! "if (myPE==0)" guard, after trnsfr0 gathers decomposed fields to rank 0),
  ! and reads currently happen redundantly on every rank (each rank opens the
  ! restart file independently). MPI_COMM_SELF preserves that per-rank
  ! independence with no new collective ordering to get right.
  !
  ! Error policy: every ADIOS2 call's status, and every variable lookup, is
  ! checked; any failure (missing variable, size mismatch, unreadable file)
  ! aborts the whole job with a message naming the variable, rather than
  ! silently restarting from partially-read/garbage data.

  use mpi
  use adios2
  use param, only : IDP

  implicit none

  private

  public :: adios_init, adios_finalize
  public :: adios_open_write, adios_open_read, adios_close_engine
  public :: adios_put_i, adios_put_r, adios_put_s, adios_put_sarr
  public :: adios_put_r1, adios_put_r2, adios_put_i1
  public :: adios_has_var
  public :: adios_series, ser_hifreq, ser_diag, ser_snap
  public :: adios_series_open, adios_series_begin, adios_series_end, adios_series_close
  public :: ser_put_i, ser_put_r, ser_put_i1, ser_put_r1, ser_put_r2
  public :: adios_get_i, adios_get_r, adios_get_s, adios_get_sarr
  public :: adios_get_r1, adios_get_r2, adios_get_r2_slice, adios_get_i1

  type(adios2_adios), save :: adios_obj
  type(adios2_io), save :: io_restart
  type(adios2_engine), save :: engine_restart

  ! A "series" is a long-lived output dataset (hifreq.bp, diagnostics.bp,
  ! field_snapshots.bp) that stays open for the whole run; each output event
  ! is one step. A continuation run reopens it in append mode, so the whole
  ! chain of restarted runs accumulates in one file. Each series has its own
  ! adios2_io (the "restart" io above is reused by every dump and is cleared
  ! on each open, so it cannot be shared).
  type adios_series
     type(adios2_io) :: io
     type(adios2_engine) :: engine
     logical :: is_open = .false.
     logical :: in_step = .false.
     logical :: fresh = .false.   ! created (truncated) by this run
     integer :: nsteps = 0        ! steps written by this run
  end type adios_series

  type(adios_series), save :: ser_hifreq, ser_diag, ser_snap
  logical, save :: engine_writing = .false.

  ! Strings are stored with this leading marker so that leading blanks (e.g.
  ! ndevice = " 2D equi") survive: adios2 stores strings as trim(adjustl()).
  character(len=1), parameter :: smark = '|'

CONTAINS

  subroutine adios_fail(msg, name)

    character(*), intent(in) :: msg, name
    integer :: ierr

    write (0,'(" ADIOS2 restart I/O error: ",a," [",a,"]")') trim(msg), trim(name)
    call MPI_Abort(MPI_COMM_WORLD, 1, ierr)
    stop 1

  end subroutine adios_fail

  subroutine adios_chk(ierr, msg, name)

    integer, intent(in) :: ierr
    character(*), intent(in) :: msg, name

    if (ierr /= adios2_error_none) call adios_fail(msg, name)

  end subroutine adios_chk

  ! Looks up a variable and aborts if it is not in the open dataset.
  subroutine adios_find(var, name)

    type(adios2_variable), intent(out) :: var
    character(*), intent(in) :: name
    integer :: ierr

    call adios2_inquire_variable(var, io_restart, name, ierr)
    call adios_chk(ierr, 'inquire failed', name)
    if (.not. var%valid) call adios_fail('variable not found in restart file', name)

  end subroutine adios_find

  ! True if the open dataset contains variable `name`.
  logical function adios_has_var(name)

    character(*), intent(in) :: name
    type(adios2_variable) :: var
    integer :: ierr

    call adios2_inquire_variable(var, io_restart, name, ierr)
    adios_has_var = (ierr == adios2_error_none .and. var%valid)

  end function adios_has_var

  ! Aborts unless variable `name` has exactly nd dimensions and its shape
  ! (first nd entries of `want`, entries <0 are not checked) is compatible:
  ! shp(k) >= want(k) for k where sub(k) is true, else shp(k) == want(k).
  subroutine adios_check_shape(var, name, nd, want, sub)

    type(adios2_variable), intent(in) :: var
    character(*), intent(in) :: name
    integer, intent(in) :: nd
    integer(kind=8), intent(in) :: want(nd)
    logical, intent(in) :: sub(nd)
    integer(kind=8), allocatable :: shp(:)
    integer :: ierr, k, ndims

    call adios2_variable_ndims(ndims, var, ierr)
    call adios_chk(ierr, 'ndims query failed', name)
    if (ndims /= nd) call adios_fail('rank (number of dimensions) mismatch', name)
    allocate (shp(ndims))
    call adios2_variable_shape(shp, ndims, var, ierr)
    call adios_chk(ierr, 'shape query failed', name)
    do k = 1, nd
       if (sub(k)) then
          if (shp(k) < want(k)) call adios_fail('stored extent smaller than requested', name)
       else
          if (shp(k) /= want(k)) call adios_fail('stored extent differs from array size', name)
       end if
    end do

  end subroutine adios_check_shape

  subroutine adios_init(comm)

    integer, intent(in) :: comm
    integer :: ierr

    call adios2_init(adios_obj, "adios2.xml", comm, ierr)
    call adios_chk(ierr, 'adios2_init failed (adios2.xml)', 'adios2.xml')
    call adios2_declare_io(io_restart, adios_obj, "restart", ierr)
    call adios_chk(ierr, 'declare_io failed', 'restart')

  end subroutine adios_init

  subroutine adios_finalize

    integer :: ierr

    call adios_series_close(ser_hifreq)
    call adios_series_close(ser_diag)
    call adios_series_close(ser_snap)
    call adios2_finalize(adios_obj, ierr)
    call adios_chk(ierr, 'adios2_finalize failed', 'finalize')

  end subroutine adios_finalize

  ! append=.true. adds a new step to an existing dataset (created if absent)
  ! instead of truncating it; the caller brackets the puts in this step.
  subroutine adios_open_write(filename, append)

    character(*), intent(in) :: filename
    logical, intent(in), optional :: append
    logical :: app
    integer :: ierr

    app = .false.
    if (present(append)) app = append

    ! io_restart is reused across every wrdump() call (one per dump file);
    ! drop any variable/attribute definitions left over from the previous
    ! dump before defining this dump's set under the same names.
    call adios2_remove_all_variables(io_restart, ierr)
    call adios2_remove_all_attributes(io_restart, ierr)

    if (app) then
       call adios2_open(engine_restart, io_restart, trim(filename), adios2_mode_append, ierr)
    else
       call adios2_open(engine_restart, io_restart, trim(filename), adios2_mode_write, ierr)
    end if
    call adios_chk(ierr, 'cannot open for write', filename)
    call adios2_begin_step(engine_restart, ierr)
    call adios_chk(ierr, 'begin_step failed', filename)
    engine_writing = .true.

  end subroutine adios_open_write

  subroutine adios_open_read(filename)

    character(*), intent(in) :: filename
    integer :: ierr

    ! io_restart may still hold variable/attribute definitions from a
    ! previous open (a prior wrdump write, or a prior read) - a BP5 reader
    ! populates them itself from the file being opened and errors ("already
    ! defined") if they're already there, so clear them first.
    call adios2_remove_all_variables(io_restart, ierr)
    call adios2_remove_all_attributes(io_restart, ierr)

    call adios2_open(engine_restart, io_restart, trim(filename), adios2_mode_readRandomAccess, ierr)
    call adios_chk(ierr, 'cannot open restart dataset for read', filename)

  end subroutine adios_open_read

  subroutine adios_close_engine

    integer :: ierr

    if (engine_writing) then
       call adios2_end_step(engine_restart, ierr)
       call adios_chk(ierr, 'end_step failed', 'engine')
       engine_writing = .false.
    end if
    call adios2_close(engine_restart, ierr)
    call adios_chk(ierr, 'close failed', 'engine')

  end subroutine adios_close_engine

  !==================  write helpers  ==================

  subroutine adios_put_i(name, val)

    character(*), intent(in) :: name
    integer, intent(in) :: val
    type(adios2_variable) :: var
    integer :: ierr

    call adios2_define_variable(var, io_restart, name, adios2_type_integer4, ierr)
    call adios_chk(ierr, 'define failed', name)
    call adios2_put(engine_restart, var, val, ierr)
    call adios_chk(ierr, 'put failed', name)

  end subroutine adios_put_i

  subroutine adios_put_r(name, val)

    character(*), intent(in) :: name
    real(IDP), intent(in) :: val
    type(adios2_variable) :: var
    integer :: ierr

    call adios2_define_variable(var, io_restart, name, adios2_type_dp, ierr)
    call adios_chk(ierr, 'define failed', name)
    call adios2_put(engine_restart, var, val, ierr)
    call adios_chk(ierr, 'put failed', name)

  end subroutine adios_put_r

  ! Scalar string; trailing blanks are not preserved (the reader re-pads),
  ! leading blanks are (via smark).
  subroutine adios_put_s(name, val)

    character(*), intent(in) :: name
    character(*), intent(in) :: val
    type(adios2_variable) :: var
    integer :: ierr

    call adios2_define_variable(var, io_restart, name, adios2_type_string, ierr)
    call adios_chk(ierr, 'define failed', name)
    call adios2_put(engine_restart, var, smark//trim(val), ierr)
    call adios_chk(ierr, 'put failed', name)

  end subroutine adios_put_s

  ! Character array, stored as one scalar string per element (name_1, name_2,
  ! ...). The ADIOS2 Fortran string-array attribute mangles blank-padded
  ! fixed-length elements, so it is not used.
  subroutine adios_put_sarr(name, val)

    character(*), intent(in) :: name
    character(*), dimension(:), intent(in) :: val
    character(len=16) :: idx
    integer :: i

    do i = 1, size(val)
       write (idx,'(i0)') i
       call adios_put_s(trim(name)//"_"//trim(idx), val(i))
    end do

  end subroutine adios_put_sarr

  subroutine adios_put_r1(name, val)

    character(*), intent(in) :: name
    real(IDP), dimension(:), intent(in) :: val
    type(adios2_variable) :: var
    integer(kind=8) :: shp(1)
    integer :: ierr

    shp(1) = size(val,1)
    call adios2_define_variable(var, io_restart, name, adios2_type_dp, 1, &
         shp, (/0_8/), shp, adios2_constant_dims, ierr)
    call adios_chk(ierr, 'define failed', name)
    call adios2_put(engine_restart, var, val, ierr)
    call adios_chk(ierr, 'put failed', name)

  end subroutine adios_put_r1

  subroutine adios_put_r2(name, val)

    character(*), intent(in) :: name
    real(IDP), dimension(:,:), intent(in) :: val
    type(adios2_variable) :: var
    integer(kind=8) :: shp(2)
    integer :: ierr

    shp(1) = size(val,1)
    shp(2) = size(val,2)
    call adios2_define_variable(var, io_restart, name, adios2_type_dp, 2, &
         shp, (/0_8,0_8/), shp, adios2_constant_dims, ierr)
    call adios_chk(ierr, 'define failed', name)
    call adios2_put(engine_restart, var, val, ierr)
    call adios_chk(ierr, 'put failed', name)

  end subroutine adios_put_r2

  subroutine adios_put_i1(name, val)

    character(*), intent(in) :: name
    integer, dimension(:), intent(in) :: val
    type(adios2_variable) :: var
    integer(kind=8) :: shp(1)
    integer :: ierr

    shp(1) = size(val,1)
    call adios2_define_variable(var, io_restart, name, adios2_type_integer4, 1, &
         shp, (/0_8/), shp, adios2_constant_dims, ierr)
    call adios_chk(ierr, 'define failed', name)
    call adios2_put(engine_restart, var, val, ierr)
    call adios_chk(ierr, 'put failed', name)

  end subroutine adios_put_i1


  !==================  series (multi-step) helpers  ==================

  ! append=.false. creates/truncates the file (new run); .true. adds steps to
  ! the existing one (continuation run).
  subroutine adios_series_open(s, ioname, filename, append)

    type(adios_series), intent(inout) :: s
    character(*), intent(in) :: ioname, filename
    logical, intent(in) :: append
    integer :: ierr

    call adios2_declare_io(s%io, adios_obj, ioname, ierr)
    call adios_chk(ierr, 'declare_io failed', ioname)
    if (append) then
       call adios2_open(s%engine, s%io, trim(filename), adios2_mode_append, ierr)
    else
       call adios2_open(s%engine, s%io, trim(filename), adios2_mode_write, ierr)
    end if
    call adios_chk(ierr, 'cannot open series for write', filename)
    s%is_open = .true.
    s%in_step = .false.
    s%fresh = .not. append
    s%nsteps = 0

  end subroutine adios_series_open

  subroutine adios_series_begin(s)

    type(adios_series), intent(inout) :: s
    integer :: ierr

    if (.not. s%is_open) return
    call adios2_begin_step(s%engine, ierr)
    call adios_chk(ierr, 'begin_step failed', 'series')
    s%in_step = .true.

  end subroutine adios_series_begin

  subroutine adios_series_end(s)

    type(adios_series), intent(inout) :: s
    integer :: ierr

    if (.not. s%is_open .or. .not. s%in_step) return
    call adios2_end_step(s%engine, ierr)
    call adios_chk(ierr, 'end_step failed', 'series')
    s%in_step = .false.
    s%nsteps = s%nsteps + 1

  end subroutine adios_series_end

  subroutine adios_series_close(s)

    type(adios_series), intent(inout) :: s
    integer :: ierr

    if (.not. s%is_open) return
    if (s%in_step) call adios_series_end(s)
    call adios2_close(s%engine, ierr)
    call adios_chk(ierr, 'close failed', 'series')
    s%is_open = .false.

  end subroutine adios_series_close

  ! Defines the variable on first use (per run) and returns it.
  subroutine ser_var(s, var, name, atype, nd, shp)

    type(adios_series), intent(inout) :: s
    type(adios2_variable), intent(out) :: var
    character(*), intent(in) :: name
    integer, intent(in) :: atype, nd
    integer(kind=8), intent(in) :: shp(:)
    integer(kind=8) :: st(max(nd,1))
    integer :: ierr

    ! inquire reports "not found" through var%valid (some builds also set
    ! ierr), so only valid matters here; define errors are checked below.
    call adios2_inquire_variable(var, s%io, name, ierr)
    if (var%valid) return
    if (nd == 0) then
       call adios2_define_variable(var, s%io, name, atype, ierr)
    else
       st = 0
       call adios2_define_variable(var, s%io, name, atype, nd, shp(1:nd), st(1:nd), shp(1:nd), &
            adios2_constant_dims, ierr)
    end if
    call adios_chk(ierr, 'define failed', name)

  end subroutine ser_var

  subroutine ser_put_i(s, name, val)

    type(adios_series), intent(inout) :: s
    character(*), intent(in) :: name
    integer, intent(in) :: val
    type(adios2_variable) :: var
    integer :: ierr

    if (.not. s%is_open) return
    call ser_var(s, var, name, adios2_type_integer4, 0, (/0_8/))
    call adios2_put(s%engine, var, val, adios2_mode_sync, ierr)
    call adios_chk(ierr, 'put failed', name)

  end subroutine ser_put_i

  subroutine ser_put_r(s, name, val)

    type(adios_series), intent(inout) :: s
    character(*), intent(in) :: name
    real(IDP), intent(in) :: val
    type(adios2_variable) :: var
    integer :: ierr

    if (.not. s%is_open) return
    call ser_var(s, var, name, adios2_type_dp, 0, (/0_8/))
    call adios2_put(s%engine, var, val, adios2_mode_sync, ierr)
    call adios_chk(ierr, 'put failed', name)

  end subroutine ser_put_r

  subroutine ser_put_i1(s, name, val)

    type(adios_series), intent(inout) :: s
    character(*), intent(in) :: name
    integer, dimension(:), intent(in) :: val
    type(adios2_variable) :: var
    integer :: ierr

    if (.not. s%is_open) return
    call ser_var(s, var, name, adios2_type_integer4, 1, (/int(size(val,1),8)/))
    call adios2_put(s%engine, var, val, adios2_mode_sync, ierr)
    call adios_chk(ierr, 'put failed', name)

  end subroutine ser_put_i1

  subroutine ser_put_r1(s, name, val)

    type(adios_series), intent(inout) :: s
    character(*), intent(in) :: name
    real(IDP), dimension(:), intent(in) :: val
    type(adios2_variable) :: var
    integer :: ierr

    if (.not. s%is_open) return
    call ser_var(s, var, name, adios2_type_dp, 1, (/int(size(val,1),8)/))
    call adios2_put(s%engine, var, val, adios2_mode_sync, ierr)
    call adios_chk(ierr, 'put failed', name)

  end subroutine ser_put_r1

  subroutine ser_put_r2(s, name, val)

    type(adios_series), intent(inout) :: s
    character(*), intent(in) :: name
    real(IDP), dimension(:,:), intent(in) :: val
    type(adios2_variable) :: var
    integer :: ierr

    if (.not. s%is_open) return
    call ser_var(s, var, name, adios2_type_dp, 2, (/int(size(val,1),8), int(size(val,2),8)/))
    call adios2_put(s%engine, var, val, adios2_mode_sync, ierr)
    call adios_chk(ierr, 'put failed', name)

  end subroutine ser_put_r2

  !==================  read helpers  ==================

  subroutine adios_get_i(name, val)

    character(*), intent(in) :: name
    integer, intent(out) :: val
    type(adios2_variable) :: var
    integer :: ierr

    call adios_find(var, name)
    call adios2_get(engine_restart, var, val, adios2_mode_sync, ierr)
    call adios_chk(ierr, 'get failed', name)

  end subroutine adios_get_i

  subroutine adios_get_r(name, val)

    character(*), intent(in) :: name
    real(IDP), intent(out) :: val
    type(adios2_variable) :: var
    integer :: ierr

    call adios_find(var, name)
    call adios2_get(engine_restart, var, val, adios2_mode_sync, ierr)
    call adios_chk(ierr, 'get failed', name)

  end subroutine adios_get_r

  subroutine adios_get_s(name, val)

    character(*), intent(in) :: name
    character(*), intent(out) :: val
    character(len=len(val)+1) :: buf
    type(adios2_variable) :: var
    integer :: ierr

    ! The C get call only overwrites as many bytes of buf as the stored
    ! string has - blank it first so the tail is real blanks. The first
    ! stored character is the smark, dropped here.
    buf = ' '
    call adios_find(var, name)
    call adios2_get(engine_restart, var, buf, adios2_mode_sync, ierr)
    call adios_chk(ierr, 'get failed', name)
    if (buf(1:1) /= smark) call adios_fail('string variable has no marker (not written by wrdump?)', name)
    val = buf(2:)

  end subroutine adios_get_s

  subroutine adios_get_sarr(name, val)

    character(*), intent(in) :: name
    character(*), dimension(:), intent(out) :: val
    character(len=16) :: idx
    integer :: i

    do i = 1, size(val)
       write (idx,'(i0)') i
       call adios_get_s(trim(name)//"_"//trim(idx), val(i))
    end do

  end subroutine adios_get_sarr

  ! 1-D reads take the FIRST size(val) elements of the stored array (via an
  ! explicit selection), so a caller may pass a shorter section than was
  ! dumped (resume with fewer modes: mm, nn, rs). Without the selection ADIOS2
  ! would copy the entire stored array into val and overrun it.
  subroutine adios_get_r1(name, val)

    character(*), intent(in) :: name
    real(IDP), dimension(:), intent(out) :: val
    type(adios2_variable) :: var
    integer(kind=8) :: st(1), cnt(1)
    integer :: ierr

    call adios_find(var, name)
    call adios_check_shape(var, name, 1, (/int(size(val),8)/), (/.true./))
    st(1) = 0
    cnt(1) = size(val)
    call adios2_set_selection(var, 1, st, cnt, ierr)
    call adios_chk(ierr, 'set_selection failed', name)
    call adios2_get(engine_restart, var, val, adios2_mode_sync, ierr)
    call adios_chk(ierr, 'get failed', name)

  end subroutine adios_get_r1

  subroutine adios_get_i1(name, val)

    character(*), intent(in) :: name
    integer, dimension(:), intent(out) :: val
    type(adios2_variable) :: var
    integer(kind=8) :: st(1), cnt(1)
    integer :: ierr

    call adios_find(var, name)
    call adios_check_shape(var, name, 1, (/int(size(val),8)/), (/.true./))
    st(1) = 0
    cnt(1) = size(val)
    call adios2_set_selection(var, 1, st, cnt, ierr)
    call adios_chk(ierr, 'set_selection failed', name)
    call adios2_get(engine_restart, var, val, adios2_mode_sync, ierr)
    call adios_chk(ierr, 'get failed', name)

  end subroutine adios_get_i1

  subroutine adios_get_r2(name, val)

    character(*), intent(in) :: name
    real(IDP), dimension(:,:), intent(out) :: val
    type(adios2_variable) :: var
    integer :: ierr

    call adios_find(var, name)
    call adios_check_shape(var, name, 2, (/int(size(val,1),8), int(size(val,2),8)/), &
         (/.false.,.false./))
    call adios2_get(engine_restart, var, val, adios2_mode_sync, ierr)
    call adios_chk(ierr, 'get failed', name)

  end subroutine adios_get_r2

  ! Reads a [n0, size(val,2)] box starting at (j0,0) into val - used for the
  ! radially-decomposed dynamical fields, where each rank fetches only its
  ! own mj_start:mj_end slice (j0=0,n0=mj+1 on rank 0, which holds the full
  ! radial range) directly via an ADIOS2 selection, replacing the old
  ! read-everything-then-discard-with-dummy-reads approach. The mode-number
  ! (2nd) dimension count is always size(val,2), so this also implements the
  ! "resume with fewer modes than were dumped" trim (val sized to
  ! min(lmax,lmaxo) by the caller). The box is packed contiguously into val,
  ! so size(val,1) must equal n0.
  subroutine adios_get_r2_slice(name, val, j0, n0)

    character(*), intent(in) :: name
    real(IDP), dimension(:,:), intent(out) :: val
    integer, intent(in) :: j0, n0
    type(adios2_variable) :: var
    integer(kind=8) :: st(2), cnt(2)
    integer :: ierr

    call adios_find(var, name)
    if (size(val,1) /= n0) call adios_fail('slice buffer size differs from requested row count', name)
    call adios_check_shape(var, name, 2, (/int(j0,8)+int(n0,8), int(size(val,2),8)/), &
         (/.true.,.true./))
    st(1) = j0
    st(2) = 0
    cnt(1) = n0
    cnt(2) = size(val,2)
    call adios2_set_selection(var, 2, st, cnt, ierr)
    call adios_chk(ierr, 'set_selection failed', name)
    call adios2_get(engine_restart, var, val, adios2_mode_sync, ierr)
    call adios_chk(ierr, 'get failed', name)

  end subroutine adios_get_r2_slice

END MODULE adios_io_mod
