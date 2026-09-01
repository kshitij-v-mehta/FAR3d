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

  use adios2
  use param, only : IDP

  implicit none

  private

  public :: adios_init, adios_finalize
  public :: adios_open_write, adios_open_read, adios_close_engine
  public :: adios_put_i, adios_put_r, adios_put_s, adios_put_sarr
  public :: adios_put_r1, adios_put_r2, adios_put_i1
  public :: adios_get_i, adios_get_r, adios_get_s, adios_get_sarr
  public :: adios_get_r1, adios_get_r2, adios_get_r2_slice, adios_get_i1

  type(adios2_adios), save :: adios_obj
  type(adios2_io), save :: io_restart
  type(adios2_engine), save :: engine_restart

CONTAINS

  subroutine adios_init(comm)

    integer, intent(in) :: comm
    integer :: ierr

    call adios2_init(adios_obj, "adios2.xml", comm, ierr)
    call adios2_declare_io(io_restart, adios_obj, "restart", ierr)

  end subroutine adios_init

  subroutine adios_finalize

    integer :: ierr

    call adios2_finalize(adios_obj, ierr)

  end subroutine adios_finalize

  subroutine adios_open_write(filename)

    character(*), intent(in) :: filename
    integer :: ierr

    ! io_restart is reused across every wrdump() call (one per dump file);
    ! drop any variable/attribute definitions left over from the previous
    ! dump before defining this dump's set under the same names.
    call adios2_remove_all_variables(io_restart, ierr)
    call adios2_remove_all_attributes(io_restart, ierr)

    call adios2_open(engine_restart, io_restart, trim(filename), adios2_mode_write, ierr)

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

  end subroutine adios_open_read

  subroutine adios_close_engine

    integer :: ierr

    call adios2_close(engine_restart, ierr)

  end subroutine adios_close_engine

  !==================  write helpers  ==================

  subroutine adios_put_i(name, val)

    character(*), intent(in) :: name
    integer, intent(in) :: val
    type(adios2_variable) :: var
    integer :: ierr

    call adios2_define_variable(var, io_restart, name, adios2_type_integer4, ierr)
    call adios2_put(engine_restart, var, val, ierr)

  end subroutine adios_put_i

  subroutine adios_put_r(name, val)

    character(*), intent(in) :: name
    real(IDP), intent(in) :: val
    type(adios2_variable) :: var
    integer :: ierr

    call adios2_define_variable(var, io_restart, name, adios2_type_dp, ierr)
    call adios2_put(engine_restart, var, val, ierr)

  end subroutine adios_put_r

  subroutine adios_put_s(name, val)

    character(*), intent(in) :: name
    character(*), intent(in) :: val
    type(adios2_variable) :: var
    integer :: ierr

    call adios2_define_variable(var, io_restart, name, adios2_type_string, ierr)
    call adios2_put(engine_restart, var, val, ierr)

  end subroutine adios_put_s

  subroutine adios_put_sarr(name, val)

    character(*), intent(in) :: name
    character(*), dimension(:), intent(in) :: val
    type(adios2_attribute) :: attr
    integer :: ierr

    call adios2_define_attribute(attr, io_restart, name, val, size(val), ierr)

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
    call adios2_put(engine_restart, var, val, ierr)

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
    call adios2_put(engine_restart, var, val, ierr)

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
    call adios2_put(engine_restart, var, val, ierr)

  end subroutine adios_put_i1

  !==================  read helpers  ==================

  subroutine adios_get_i(name, val)

    character(*), intent(in) :: name
    integer, intent(out) :: val
    type(adios2_variable) :: var
    integer :: ierr

    call adios2_inquire_variable(var, io_restart, name, ierr)
    call adios2_get(engine_restart, var, val, adios2_mode_sync, ierr)

  end subroutine adios_get_i

  subroutine adios_get_r(name, val)

    character(*), intent(in) :: name
    real(IDP), intent(out) :: val
    type(adios2_variable) :: var
    integer :: ierr

    call adios2_inquire_variable(var, io_restart, name, ierr)
    call adios2_get(engine_restart, var, val, adios2_mode_sync, ierr)

  end subroutine adios_get_r

  subroutine adios_get_s(name, val)

    character(*), intent(in) :: name
    character(*), intent(out) :: val
    type(adios2_variable) :: var
    integer :: ierr

    ! adios2_put_string stores TRIM(ADJUSTL(...)) of whatever was written,
    ! and the C get call below only overwrites that many bytes of val - blank
    ! it first so any tail beyond the stored string is a real blank, not
    ! whatever happened to be in val's memory before this call.
    val = ' '
    call adios2_inquire_variable(var, io_restart, name, ierr)
    call adios2_get(engine_restart, var, val, adios2_mode_sync, ierr)

  end subroutine adios_get_s

  subroutine adios_get_sarr(name, val)

    character(*), intent(in) :: name
    character(*), dimension(:), intent(out) :: val
    type(adios2_attribute) :: attr
    integer :: ierr

    call adios2_inquire_attribute(attr, io_restart, name, ierr)
    call adios2_attribute_data(val, attr, ierr)

  end subroutine adios_get_sarr

  subroutine adios_get_r1(name, val)

    character(*), intent(in) :: name
    real(IDP), dimension(:), intent(out) :: val
    type(adios2_variable) :: var
    integer :: ierr

    call adios2_inquire_variable(var, io_restart, name, ierr)
    call adios2_get(engine_restart, var, val, adios2_mode_sync, ierr)

  end subroutine adios_get_r1

  subroutine adios_get_r2(name, val)

    character(*), intent(in) :: name
    real(IDP), dimension(:,:), intent(out) :: val
    type(adios2_variable) :: var
    integer :: ierr

    call adios2_inquire_variable(var, io_restart, name, ierr)
    call adios2_get(engine_restart, var, val, adios2_mode_sync, ierr)

  end subroutine adios_get_r2

  subroutine adios_get_i1(name, val)

    character(*), intent(in) :: name
    integer, dimension(:), intent(out) :: val
    type(adios2_variable) :: var
    integer :: ierr

    call adios2_inquire_variable(var, io_restart, name, ierr)
    call adios2_get(engine_restart, var, val, adios2_mode_sync, ierr)

  end subroutine adios_get_i1

  ! Reads a [n0, size(val,2)] box starting at (j0,0) into val - used for the
  ! radially-decomposed dynamical fields, where each rank fetches only its
  ! own mj_start:mj_end slice (j0=0,n0=mj+1 on rank 0, which holds the full
  ! radial range) directly via an ADIOS2 selection, replacing the old
  ! read-everything-then-discard-with-dummy-reads approach. The mode-number
  ! (2nd) dimension count is always size(val,2), so this also implements the
  ! "resume with fewer modes than were dumped" trim (val sized to
  ! min(lmax,lmaxo) by the caller).
  subroutine adios_get_r2_slice(name, val, j0, n0)

    character(*), intent(in) :: name
    real(IDP), dimension(:,:), intent(out) :: val
    integer, intent(in) :: j0, n0
    type(adios2_variable) :: var
    integer(kind=8) :: st(2), cnt(2)
    integer :: ierr

    call adios2_inquire_variable(var, io_restart, name, ierr)
    st(1) = j0
    st(2) = 0
    cnt(1) = n0
    cnt(2) = size(val,2)
    call adios2_set_selection(var, 2, st, cnt, ierr)
    call adios2_get(engine_restart, var, val, adios2_mode_sync, ierr)

  end subroutine adios_get_r2_slice

END MODULE adios_io_mod
