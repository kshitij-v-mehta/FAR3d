module bp_read_mod

  ! Minimal serial ADIOS2 reader for the FAR3d post-processing tools: open a
  ! .bp dataset, read named scalars / 1-D / 2-D arrays. Any failure (missing
  ! file, missing variable) stops with a message naming the file and variable.
  ! Arrays are returned sized to what is stored; 2-D arrays are
  ! (first dimension) x (second dimension) as written by FAR3d, i.e.
  ! (radial 0:mj -> 1:mj+1) x (mode).

  use adios2

  implicit none
  private

  public :: bp_open, bp_close, bp_has_var
  public :: bp_get_i, bp_get_r, bp_get_i1, bp_get_r1, bp_get_r2

  type(adios2_adios), save :: adios
  type(adios2_io), save :: io
  type(adios2_engine), save :: engine
  logical, save :: is_init = .false.
  character(len=256), save :: cur_file = ' '
  integer, save :: ndataset = 0

contains

  subroutine bp_fail(msg, name)
    character(*), intent(in) :: msg, name
    write (0,'(" bp_read error (",a,"): ",a," [",a,"]")') trim(cur_file), trim(msg), trim(name)
    stop 1
  end subroutine bp_fail

  subroutine bp_chk(ierr, msg, name)
    integer, intent(in) :: ierr
    character(*), intent(in) :: msg, name
    if (ierr /= adios2_error_none) call bp_fail(msg, name)
  end subroutine bp_chk

  subroutine bp_open(filename)
    character(*), intent(in) :: filename
    character(len=16) :: ioname
    logical :: exists
    integer :: ierr

    cur_file = filename
    inquire (file=trim(filename), exist=exists)
    if (.not. exists) call bp_fail('file/dataset not found', filename)
    if (.not. is_init) then
       call adios2_init(adios, ierr)
       call bp_chk(ierr, 'adios2_init failed', 'adios2')
       is_init = .true.
    end if
    ndataset = ndataset + 1
    write (ioname,'("read",i0)') ndataset
    call adios2_declare_io(io, adios, trim(ioname), ierr)
    call bp_chk(ierr, 'declare_io failed', ioname)
    call adios2_open(engine, io, trim(filename), adios2_mode_readRandomAccess, ierr)
    call bp_chk(ierr, 'cannot open for read', filename)
  end subroutine bp_open

  subroutine bp_close
    integer :: ierr
    call adios2_close(engine, ierr)
    call bp_chk(ierr, 'close failed', 'engine')
  end subroutine bp_close

  logical function bp_has_var(name)
    character(*), intent(in) :: name
    type(adios2_variable) :: var
    integer :: ierr
    call adios2_inquire_variable(var, io, name, ierr)
    bp_has_var = (ierr == adios2_error_none .and. var%valid)
  end function bp_has_var

  subroutine find(var, name)
    type(adios2_variable), intent(out) :: var
    character(*), intent(in) :: name
    integer :: ierr
    call adios2_inquire_variable(var, io, name, ierr)
    if (ierr /= adios2_error_none .or. .not. var%valid) call bp_fail('variable not found', name)
  end subroutine find

  subroutine shape_of(var, name, nd, shp)
    type(adios2_variable), intent(in) :: var
    character(*), intent(in) :: name
    integer, intent(in) :: nd
    integer(kind=8), intent(out) :: shp(nd)
    integer(kind=8), allocatable :: s(:)
    integer :: ierr, ndims
    call adios2_variable_ndims(ndims, var, ierr)
    call bp_chk(ierr, 'ndims query failed', name)
    if (ndims /= nd) call bp_fail('unexpected number of dimensions', name)
    call adios2_variable_shape(s, ndims, var, ierr)
    call bp_chk(ierr, 'shape query failed', name)
    shp = s(1:nd)
  end subroutine shape_of

  subroutine bp_get_i(name, val)
    character(*), intent(in) :: name
    integer, intent(out) :: val
    type(adios2_variable) :: var
    integer :: ierr
    call find(var, name)
    call adios2_get(engine, var, val, adios2_mode_sync, ierr)
    call bp_chk(ierr, 'get failed', name)
  end subroutine bp_get_i

  subroutine bp_get_r(name, val)
    character(*), intent(in) :: name
    double precision, intent(out) :: val
    type(adios2_variable) :: var
    integer :: ierr
    call find(var, name)
    call adios2_get(engine, var, val, adios2_mode_sync, ierr)
    call bp_chk(ierr, 'get failed', name)
  end subroutine bp_get_r

  subroutine bp_get_i1(name, val)
    character(*), intent(in) :: name
    integer, allocatable, intent(out) :: val(:)
    type(adios2_variable) :: var
    integer(kind=8) :: shp(1)
    integer :: ierr
    call find(var, name)
    call shape_of(var, name, 1, shp)
    allocate (val(shp(1)))
    call adios2_get(engine, var, val, adios2_mode_sync, ierr)
    call bp_chk(ierr, 'get failed', name)
  end subroutine bp_get_i1

  subroutine bp_get_r1(name, val)
    character(*), intent(in) :: name
    double precision, allocatable, intent(out) :: val(:)
    type(adios2_variable) :: var
    integer(kind=8) :: shp(1)
    integer :: ierr
    call find(var, name)
    call shape_of(var, name, 1, shp)
    allocate (val(shp(1)))
    call adios2_get(engine, var, val, adios2_mode_sync, ierr)
    call bp_chk(ierr, 'get failed', name)
  end subroutine bp_get_r1

  subroutine bp_get_r2(name, val)
    character(*), intent(in) :: name
    double precision, allocatable, intent(out) :: val(:,:)
    type(adios2_variable) :: var
    integer(kind=8) :: shp(2)
    integer :: ierr
    call find(var, name)
    call shape_of(var, name, 2, shp)
    allocate (val(shp(1), shp(2)))
    call adios2_get(engine, var, val, adios2_mode_sync, ierr)
    call bp_chk(ierr, 'get failed', name)
  end subroutine bp_get_r2

end module bp_read_mod
