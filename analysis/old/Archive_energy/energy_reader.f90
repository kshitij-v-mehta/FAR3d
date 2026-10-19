      implicit none
      character*1 :: chr
      character*1 :: tb
      real*16, allocatable :: eke(:,:)
      real*16, allocatable :: t(:)
      real*16, allocatable :: sum_e(:,:)
      integer, allocatable :: mode_address(:,:)
      integer :: m,n,j,i,ii,times,index,num_modes,upper_n,lower_n,max_m
      integer :: modes(0:15)
      integer :: nn(0:15)
      data nn/0,2,3,4,5,6,7,8,9,10,11,12,1,13,14,15/
      data modes/50,10,13,18,21,26,30,32,35,37,42,45,6,51,54,57/
      open(unit=8,file="ekenc_all",status="old")
      times = 2180
      upper_n = maxval(nn)
      lower_n = minval(nn)
      num_modes = sum(modes)
      max_m = maxval(modes)
      allocate(t(times))
      allocate(eke(times,num_modes))
      allocate(mode_address(lower_n:upper_n,max_m))
      allocate(sum_e(times,lower_n:upper_n))
      write(*,*) num_modes,upper_n,lower_n, times
      read(8,'(a1)') chr
      do j = 2,times
       read(8,*) t(j), (eke(j,i), i=1,num_modes)
      end do

      mode_address(:,:) = 0
      index = 0
      do n=lower_n,upper_n
       do m=1,modes(n)
        index = index + 1
	    mode_address(n,m) = index
!	    write(*,*) n, m, index
       end do
      end do
      
	do j=2,times  !time loop
       do n=lower_n,upper_n  !tor mode index loop
        sum_e(j,n) = 0.
        do m=1,modes(n)   !poloidal mode loop
	     ii = mode_address(n,m)
	     sum_e(j,n) = sum_e(j,n) + eke(j,ii)
	    end do
       end do
      end do
      tb = char(9)
      open(unit=9,file="energy_vs_tor_mode.dat",status="unknown")
       write(9,'("time",21(a1,i3))') (tb,nn(n), n=lower_n,upper_n)
       do j=1,times
        write(9,'(e15.8,21(a1,e15.8))') t(j),(tb,sum_e(j,n), n=lower_n,upper_n)
       end do
       
!      do n=lower_n,upper_n
!       write(*,*) nn(n),sum_e(times,n)
!      end do
      stop
      end
