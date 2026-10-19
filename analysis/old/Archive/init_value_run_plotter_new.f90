!   This (v2) version of the plotter for nonlinear runs makes use of the new geom_bzr_to_cyl file which is written out
!    from within sub. vmec and is on the same radial grid as the far3d run, so no awkward interpolation has to be done.
!
	module param
!		(jdim,ldim) should be equal to (mj,lmax)
		implicit none
		integer, parameter :: IDP = kind(1.d0)
		integer, parameter :: leqdim=50, ldim=1053, jdim=460
	end module param

	module cotrol
		use param
		implicit none
		save
		character(len=8), dimension(:), allocatable :: numhist
		character(len=2), dimension(3) :: numrun,numruno,numruns
		character(len=2) :: numdia
		character(len=5) :: numvac
		real(IDP) :: stdifp,stdifu,stdifnf,stdifvf,stdifv,stdifnalp,stdifvalp,dt0,ext_prof,LcA0,LcA1,LcA2,LcA3,dpres,omegar, &
			     iflr,r_epflr,LcA0alp,LcA1alp,LcA2alp,LcA3alp,r_epflralp
		integer :: noeqn,nvar,numeq,ihist,nocpl,ifreezd,ifreezu,maxstp,nstep,ndump,ndiag,nprint,lplots,itime, &
			   nstep1,nonlin,jmn,jmx,jint,ndis,nwdth,twofl_on,epflr_on,alpha_on,iflr_on,ieldamp_on,ivalp, &
			   iq,iw,ix1,ix2,iwa,ix1a,ix2a,spe1,spe2
		integer :: variable, zonal_plot
		character(len=8) :: rdump_file_name		
	end module cotrol

	module domain
		use param
		implicit none
		save
		integer :: mjeq,nfp
		real(IDP), dimension(:), allocatable :: rfar,qfar
		integer :: mj,lmax,leqmax,lbmax,mmin,mmax,nmin,nmax,mmineq,mmaxeq,nmineq,nmaxeq,mjm1,mjm2,mjp1,lmax0,lmaxn,lmx,l0, &
		           leq0,lhmax,lheqmx,nnum,m0dy,mxmband,mbandeq,nnd,nst,mnxxx,mminb,mmaxb,nminb,nmaxb,mxmbandb,mmaxx,mmaxxb,mmaxxeq
		integer, dimension(ldim) :: mm,nn,mh,nh
		integer, dimension(leqdim) :: mmeq,nneq,mheq,nheq
		integer, dimension(:), allocatable :: mmb,nnb
		integer, dimension(:), allocatable :: mmstart,mmend,mmstreq,mmendeq
		integer, dimension(:), allocatable :: mmstartb,mmendb
		real(IDP), dimension(0:jdim) :: r,rinv
		real(IDP), dimension(jdim) :: dc1m,dc1p,dc2m,dc2p,del2cm,del2cp
		real(IDP), dimension(jdim,2) :: wt1m,wt10,wt1p,wt2m,wt20,wt2p
		integer, dimension(:,:), allocatable :: ll
		integer, dimension(:,:), allocatable :: llb
		integer, dimension(:,:), allocatable :: lleq
		integer, dimension(:), allocatable :: m1n,mrang
		integer, dimension(:), allocatable :: ll0
		integer, dimension(:), allocatable :: lln,lo,llno
		integer, dimension(ldim) :: signl,jsl
		integer, dimension(leqdim) :: sgnleq
		integer, dimension(:), allocatable :: lnumn
		integer, dimension(:), allocatable :: mnumn,nskpn,nskpxn,nskpin,nskp2n
		integer :: ni,nis,ne
		real(IDP) :: pi,delta,rc,fti,fte
!		character(len=10) :: timew,datew
!		real :: time_sc,time_ec,time_c,time_sm,time_em,time_m
	end module domain

	module equil
		use param
		implicit none
		save
		real(IDP), dimension(0:jdim) :: qq,qqinv,qqinvp,denseq,denseqr,psieq,chieq,preq,feq,cureq,teeq,tieq,nfeq,dnfeqdr,vfova,vfova2, &
						vzt_eq,vtherm_elc,nalpeq,valphaova,valphaova2,dnalpeqdr,edge_sprs
		real(IDP), dimension(:,:), allocatable :: ceqt,ceqz,fex,psiex,curex
		character(len=8), dimension(2) :: ndevice
		integer :: ngeneq,ieq,nqprof,leq,ndat,ltwo,ntok
		real(IDP) :: eps,epst,bet0,q0,qlamb,ro,qh,rsbar,qlambpp,pop,popp,pol2,betaj,betat,betap,xle,omcy,omcyalp,bet0_f,bet0_alp, &
			     Adens,Bdens,Adensalp,Bdensalp,uion,vthi,vthe,xnuelc0,coul_log
		real(IDP), dimension(:), allocatable :: dnnbi,dne,dni,ti,te,temp_ep,vAlfven,vtherm_elcP,qprofile,pthermal, &
							pthermalnn,vthermalep,dnnbinn,dnenn,dninn,tinn,tenn,temp_epnn,tbn,tbnnn, &
							dnalpha,dnalphann,talpha,talphann		
		real(IDP), dimension(ldim) :: rs
		real(IDP), dimension(100) :: xr,xdat,pdat
		real(IDP), dimension(0:10) :: cnep,ctep,cvep,cnfp,cvfp,cnfpalp,cvfpalp
		real(IDP), dimension(:,:), allocatable :: R_eq,Z_eq                    !Added R_eq, Z_eq 9-19-2022
		real(IDP), dimension(:,:), allocatable :: bigr,bigr2,bigri,bigr2i,grr,grt,gtt,r2grr,r2grt,r2gtt,xeq,zeq,xeqr,zeqr,xeqt, &
							  zeqt,sqgi,sqg,bmod,grroj,grtoj,gttoj,bst,jbgrr,jbgrt,jbgtt,lplr,lplt,lplz,djroj, &
							  djtoj,djzoj,omdr,omdt,omdz,dbsjtoj,dbsjzoj,dbsjtbj,dgttr,dgrrt,dgrtt,dgttt,dgrrz, &
							  dgrtz,dgttz,dgrtp,dgttp,jsq,bsgrt,bsgtt,bsq,bsqgtt,lplrr,lplrt,lplrz,lpltr,lpltt, &
							  lpltz,lplzr,lplzt,lplzz,eildr,eildt,eildz,eildrr,eildrt,eildrz,eildtt,eildtz,eildzz, &
							  omdrprp,omdtprp,omdzprp
	end module equil

	module dynamo
		use param
		implicit none
		save
		integer :: ietaeq,ichipr,ikp,ipert,mfbk,mhfb,nhfb,nslice,lmaxnm1,lmaxv
		real(IDP) :: dt,dtd2,time,etascl,reta,eta0,etalmb,chipr,rchi,s,gamma,pertscl,wsrc,rsrc,asrc
		real(IDP), dimension(0:jdim) :: eta,chiprp
		real(IDP), dimension(:), allocatable :: chiu,chipres,tchi,src
		real(IDP), dimension(:), allocatable :: difu,difp,twodif		
		real(IDP), dimension(:,:), allocatable :: psi,phi,pr,uzt,nf,vprlf,vthprlf,nalp,vprlalp,psip,prp,uztp,nfpp,vprlfp,vthprlfp, &
							  nalpp,vprlalpp, plot_var
		real(IDP), dimension(:,:,:), allocatable :: psi_nl,pr_nl,uzt_nl,nf_nl,vprlf_nl,vthprlf_nl,nalp_nl,vprlalp_nl
		real(IDP), dimension(100) :: etadat
		real(IDP), dimension(ldim) :: widthi,gammai,psibnd
		real(IDP), dimension(:,:), allocatable :: cmamm,cmamp,cmapm,cmapp
		real(IDP), dimension(:), allocatable :: amat,bmat,cmat
		real(IDP), dimension(:,:,:), allocatable :: amatw,bmatw,cmatw,amatwalp,bmatwalp,cmatwalp
		real(IDP), dimension(:), allocatable :: xt,yt
		real(IDP), dimension(:,:), allocatable :: xw
		integer, dimension(:), allocatable :: ipc
		integer, dimension(:,:), allocatable :: ipcw,ipcwalp
		real(IDP), dimension(:), allocatable :: widthh,phased
	end module dynamo

	module scratch
		use param
		implicit none
		save
		real(IDP), dimension(:,:), allocatable :: sc1,sc2,sc3,sc4,sc5,sc6,sc7,sc8,sc9,sc10,sc11,sc12
 		real(IDP), dimension(0:jdim,0:leqdim) :: sceq1,sceq2,sceq3,sceq4,sceq5,sceq6
		real(IDP), dimension(0:jdim) :: sd1,sd2,sd3,sd4,sd5,sd6,sd7
	end module scratch

	module findrf
		use param
		implicit none
		save
		integer :: n0,nt
		real(IDP) :: d,dx
	end module findrf


	program far

		use param
		use cotrol
		use domain
		use equil
		use dynamo
		use scratch
		implicit none

		namelist/nam/maxstp,ndump,ndiag,nprint,lplots,itime,dt0,nonlin,mj,lmax,leqmax,mm,nn,mmeq,nneq,jmn,jmx,jint,ni,nis,ne, &
			     delta,rc,fti,fte,stdifp,stdifu,stdifv,ngeneq,ieq,eps,epst,bet0,nqprof,q0,qlamb,ro,rs,leq,qh,rsbar,qlambpp, &
			     ndat,xr,xdat,pop,popp,pol2,ltwo,betaj,pdat,ntok,etascl,reta,eta0,etalmb,ietaeq,etadat,chipr,ichipr,rchi, &
			     s,gamma,ipert,widthi,gammai,pertscl,m0dy,nocpl,ifreezd,ifreezu,xle,wsrc,rsrc,asrc,cnep,ctep,cvep,omcy,bet0_f, &
			     cnfp,cvfp,stdifnf,stdifvf,ext_prof,epflr_on,r_epflr,alpha_on,iflr_on,iflr,LcA0,LcA1,LcA2,LcA3,Adens,Bdens, &
			     twofl_on,dpres,bet0_alp,Adensalp,Bdensalp,LcA0alp,LcA1alp,LcA2alp,LcA3alp,omcyalp,r_epflralp,stdifnalp,stdifvalp, &
			     omegar,ieldamp_on,ndis,nwdth,spe1,spe2

		character(len=132) :: char5
		character(len=10) :: timew,datew,zone_h
		integer, dimension(8) :: values_s,values_e
		character(len=16) :: confil
		character(len=5) :: numeqc
		integer :: count
		character(len=1) :: t
		integer :: nstres,i,l,j,iend,l2t,mnum,ierror
		real(IDP) :: d,dk,scmn,scmx,sclmn,sclmx,scnorm
                character*8 :: arg1
                character*2 :: arg2

		interface
			subroutine dfault
			end subroutine dfault
			subroutine inital
			end subroutine inital
			subroutine resume
			end subroutine resume
			subroutine linstart
			end subroutine linstart
			subroutine linstep
			end subroutine linstep
			subroutine lincheck
			end subroutine lincheck
			subroutine output
			end subroutine output
			subroutine wrdump
			end subroutine wrdump
			subroutine endrun
			end subroutine endrun			
			subroutine numinc
			end subroutine numinc
			subroutine energy(i)
				implicit none
				integer :: i
			end subroutine energy
			subroutine elapsed_time(values_s,values_e)
				implicit none
				integer, dimension(8) :: values_s,values_e
			end subroutine elapsed_time
		end interface

		variable = 2; rdump_file_name="fs0000z"
		count = command_argument_count()
		if(count .eq. 1) then
		 call getarg(1, arg1)
                 rdump_file_name=trim(adjustl(arg1))
		else if(count .eq. 2) then
		 call getarg(1, arg1)
                 rdump_file_name=trim(adjustl(arg1))
                 call getarg(2, arg2)
                 read(arg2,'(i2)') variable
		end if
!		write(0,'(/)')
		write(0,'(" READING FROM ",a8," dump-file")')rdump_file_name
		if(variable .eq. 1) write(0,'("  PLOTTING 2D psi contours")')
		if(variable .eq. 2) write(0,'("  PLOTTING 2D phi contours")')
		if(variable .eq. 3) write(0,'("  PLOTTING 2D pressure contours")')
		if(variable .eq. 4) write(0,'("  PLOTTING 2D n_fast contours")')
		if(variable .eq. 5) write(0,'("  PLOTTING 2D v_fast_parallel contours")')
		if(variable .eq. 6) write(0,'("  PLOTTING 2D vth_parallel contours")')
!		Input read	 
		write(0,'(" ====> Checking input list ... ")')		
		open (unit=5,file="in",status="old")
		
		zonal_plot = 1       !Set to 0 for normal mode structure plots, = 1 for zonal plots (n = 0 only)
		
		read(5,'(i1,2x,2a2,2x,2a2,a1,2x,a5)') nstres,(numrun(i),i=1,2),(numruno(i),i=1,3),numeqc
		rewind(5)

		t=char(9)

		confil="farprt"//numrun(1)//numrun(2)
		call dfault

		read(5,'(i1,2x,2a2,2x,2a2,a1,2x,a5)') nstres,(numrun(i),i=1,2),(numruno(i),i=1,3),numeqc

		if (nstres /= 0) then

!			confil="fs"//numruno(1)//numruno(2)//numruno(3)
!			open(unit=8,file=confil,status='old',convert='big_endian',form='unformatted')
			open(unit=8,file=rdump_file_name,status='old',convert='big_endian',form='unformatted')

			read(8) ihist
			rewind(8)

		end if
		read(5,nam)

		noeqn=7
		ivalp=0
		iq=0
		iw=0
		ix1=0
		ix2=0
		iwa=0
		ix1a=0
		ix2a=0
		if (alpha_on == 1) then
			noeqn=noeqn+2
			ivalp=noeqn
		end if
		nvar=noeqn
		if (iflr_on == 1) noeqn=noeqn+1
		iq=noeqn
		if (epflr_on == 1) then
			if (r_epflr > 0.0) then
				iw=noeqn+1
				ix1=noeqn+2
				noeqn=noeqn+3
				ix2=noeqn
			end if
			if (alpha_on == 1 .and. r_epflralp > 0.0) then
				iwa=noeqn+1
				ix1a=noeqn+2
				noeqn=noeqn+3
				ix2a=noeqn
			end if
		end if

		if (ext_prof == 0) ieldamp_on = 0		

		ihist=ihist+1

		allocate (numhist(ihist))
                call resume


	end program far

	subroutine dfault

		use param
		use cotrol
		use domain
		use equil
		use dynamo
		implicit none

		numrun(1)="zz"
		numrun(2)="zz"
		numrun(3)="0"
		numruno(1)="fs"
		numruno(2)="##"
		numruno(3)="#"
		numruns(1)="zz"
		numruns(2)="zz"
		numruns(3)="0"
		numdia="00"
		numeq=0
		numvac="vvvvv"
		ihist=0
		maxstp=100000
		nstep=0
		ndump=100000
		nprint=100
		lplots=0
		itime=1
		dt0=.66
		nstep1=0
		nonlin=1
		mj=jdim
		lmax=0
		leqmax=leqdim
		mmin=0
		mmax=0
		nmin=0
		nmax=0
		mmineq=0
		mmaxeq=0
		nmineq=0
		nmaxeq=0
		mjm1=mj-1
		mjm2=mj-2		
		mjp1=mj+1
		r=0.
		mm=0
		nn=0
		mmeq=0
		nneq=0
		l0=0
		leq0=0
		rinv=0.
		dc1m=0.
		dc1p=0.
		dc2m=0.
		dc2p=0.
		del2cm=0.
		del2cp=0.
		signl=0
		sgnleq=0
		lhmax=0
		mh=0
		nh=0
		lheqmx=0
		mheq=0
		nheq=0
		m0dy=0
		nocpl=0
		ni=mj/3
		ne=mj/3
		nis=mj-ni-ne
		delta=.333
		Adens=7.0
		Bdens=0.5		
		LcA0=0.5
		LcA1=-0.88623
		LcA2=1
		LcA3=1	
		ext_prof=0	
		epflr_on=0		
		rc=.5
		fti=.95
		fte=.95
		qq=0.
		qqinv=0.
		psieq=0.
		chieq=0.
		preq=0.
		feq=0.
		cureq=0.
		denseq=1.		
!		ceqt=0.
!		ceqz=0.
!		teeq=1.		
		ngeneq=0
		ndevice(1)="        "
		ndevice(2)="        "
		ieq=1
		eps=0.
		epst=0.
		bet0=1.
		nqprof=12
		q0=.9
		qlamb=2.
		ro=.6
		rs=0.
		leq=1
		qh=0.
		rsbar=0.
		qlambpp=0.
		ndat=0
		xr=0.
		xdat=0.
		pop=-1.
		popp=1.
		pol2=-pop-popp
		ltwo=5
		betaj=.5
		pdat=0.
		ntok=1
		nfp=1		
		betat=0.
		betap=0.
		ifreezd=0
		ifreezu=0
		xle=0.
!		bigr=0.
!		bigr2=0.
!		bigri=0.
!		bigr2i=0.
!		grr=0.
!		grt=0.
!		gtt=0.
!		r2grr=0.
!		r2grt=0.
!		r2gtt=0.
!		xeq=0.
!		zeq=0.
!		xeqr=0.
!		zeqr=0.
!		xeqt=0.
!		zeqt=0.
		eta=0.
		chiprp=0.
		dt=0.
		time=0.
		etascl=1.
		reta=0.
		eta0=0.
		etalmb=0.
		ietaeq=0
		etadat=0.
		chipr=0.
		ichipr=2
		rchi=0.
		s=1.e+5
		gamma=0.
		ipert=0
		widthi=0.
		gammai=0.
		pertscl=1.0_IDP
		stdifv=0.
		cnep=0.
		ctep=0.
		ndis=18
		nwdth=3		
		asrc=0.
		rsrc=0.45
		wsrc=0.05
		stdifp=0.
		stdifu=0.
		stdifnf=0.
		stdifvf=0.
		cnep=0.
		ctep=0.
		cvep=0.

	end subroutine dfault
	
	subroutine resume

		use param
		use cotrol
		use domain
		use equil
		use dynamo
		use scratch
		implicit none

		namelist/nam/maxstp,ndump,ndiag,nprint,lplots,itime,dt0,nonlin,mj,lmax,leqmax,mm,nn,mmeq,nneq,jmn,jmx,jint,ni,nis,ne, &
			     delta,rc,fti,fte,stdifp,stdifu,stdifv,ngeneq,ieq,eps,epst,bet0,nqprof,q0,qlamb,ro,rs,leq,qh,rsbar,qlambpp, &
			     ndat,xr,xdat,pop,popp,pol2,ltwo,betaj,pdat,ntok,etascl,reta,eta0,etalmb,ietaeq,etadat,chipr,ichipr,rchi, &
			     s,gamma,ipert,widthi,gammai,pertscl,m0dy,nocpl,ifreezd,ifreezu,xle,wsrc,rsrc,asrc,cnep,ctep,cvep,omcy,bet0_f, &
			     cnfp,cvfp,stdifnf,stdifvf,ext_prof,epflr_on,r_epflr,alpha_on,iflr_on,iflr,LcA0,LcA1,LcA2,LcA3,Adens,Bdens, &
			     twofl_on,dpres,bet0_alp,Adensalp,Bdensalp,LcA0alp,LcA1alp,LcA2alp,LcA3alp,omcyalp,r_epflralp,stdifnalp,stdifvalp, &
			     omegar,ieldamp_on,ndis,nwdth,spe1,spe2

		integer :: nstres,i,j,l
		real(IDP) :: dum
		character(len=5) :: numeqc

		interface
			subroutine rddump(ir)
			  implicit none
			  integer :: ir
			end subroutine rddump
			subroutine setmod
			end subroutine setmod
			subroutine mmlims
			end subroutine mmlims
			subroutine etachi
			end subroutine etachi
			subroutine pert
			end subroutine pert
		end interface

		call rddump(1)

!		rewind(5)
!		read(5,'(i1,2x,2a2,2x,2a2,a1,2x,a5)') nstres,(numrun(i),i=1,2),(numruno(i),i=1,3),numeqc
!		read(5,nam)
!		rewind(5)

		call setmod
		call mmlims

		allocate (psi(0:mj,0:lmax),phi(0:mj,0:lmax),uzt(0:mj,0:lmax),pr(0:mj,0:lmax),nf(0:mj,0:lmax),vprlf(0:mj,0:lmax), &
			  vthprlf(0:mj,0:lmax))
		allocate (psip(0:mj,0:lmax),uztp(0:mj,0:lmax),prp(0:mj,0:lmax),nfpp(0:mj,0:lmax),vprlfp(0:mj,0:lmax), &
			  vthprlfp(0:mj,0:lmax))
		allocate (psi_nl(0:mj,0:lmax,3),uzt_nl(0:mj,0:lmax,3),pr_nl(0:mj,0:lmax,3),nf_nl(0:mj,0:lmax,3),vprlf_nl(0:mj,0:lmax,3), &
			  vthprlf_nl(0:mj,0:lmax,3))
		allocate (sc1(0:mj,0:lmax),sc2(0:mj,0:lmax),sc3(0:mj,0:lmax),sc4(0:mj,0:lmax),sc5(0:mj,0:lmax),sc6(0:mj,0:lmax), &
			  sc7(0:mj,0:lmax),sc8(0:mj,0:lmax),sc9(0:mj,0:lmax),sc10(0:mj,0:lmax))

		psi=0.0_IDP
		phi=0.0_IDP
		pr=0.0_IDP
		nf=0.0_IDP
		vprlf=0.0_IDP
		vthprlf=0.0_IDP
		psip=0.0_IDP
		prp=0.0_IDP
		nfpp=0.0_IDP
		vprlfp=0.0_IDP
		vthprlfp=0.0_IDP
		psi_nl=0.0_IDP
		uzt_nl=0.0_IDP
		pr_nl=0.0_IDP
		nf_nl=0.0_IDP
		vprlf_nl=0.0_IDP
		vthprlf_nl=0.0_IDP
	write(*,'("alpha_on =",l)') alpha_on
		if (alpha_on == 1) then
			allocate (nalp(0:mj,0:lmax),vprlalp(0:mj,0:lmax),nalpp(0:mj,0:lmax),vprlalpp(0:mj,0:lmax))
			allocate (sc11(0:mj,0:lmax),sc12(0:mj,0:lmax))
			allocate (nalp_nl(0:mj,0:lmax,3),vprlalp_nl(0:mj,0:lmax,3))
			nalp=0.0_IDP
			vprlalp=0.0_IDP
			nalpp=0.0_IDP
			vprlalpp=0.0_IDP
			nalp_nl=0.0_IDP
			vprlalp_nl=0.0_IDP
		end if
		
		sc1=0.
		sc2=0.
		sc3=0.
		sc4=0.
		sc5=0.
		sc6=0.
		sc7=0.
		sc8=0.
		sc9=0.
		sc10=0.
		read(8) ((psi(j,l),j=0,mj),l=1,lmax)
		read(8) ((phi(j,l),j=0,mj),l=1,lmax)
		read(8) ((pr(j,l),j=0,mj),l=1,lmax)
		read(8) ((nf(j,l),j=0,mj),l=1,lmax)
		read(8) ((vprlf(j,l),j=0,mj),l=1,lmax)
		read(8) ((vthprlf(j,l),j=0,mj),l=1,lmax)
	write(*,'("alpha_on =",l)') alpha_on
		if (alpha_on == 1) then
			read(8) ((nalp(j,l),j=0,mj),l=1,lmax)
			read(8) ((vprlalp(j,l),j=0,mj),l=1,lmax)
		end if
		
		read(8) etascl,reta,eta0,etalmb,ietaeq,(etadat(j),j=1,ndat),chipr,ichipr,rchi,stdifp,stdifu,stdifnf,stdifv,stdifvf, &
			stdifnalp,stdifvalp,s,gamma,xle,ipert,(dum,l=1,lmax),(dum,l=1,lmax),dum

		rewind(8)
		close(unit=8)

		rewind(5)
		read(5,'(i1,2x,2a2,2x,2a2,a1,2x,a5)') nstres,(numrun(i),i=1,2),(numruno(i),i=1,3),numeqc
		read(5,nam)
		rewind(5)
			
		call rddump(2)
!		call ae_profiles
!		call etachi
!		call pert

	end subroutine resume

	subroutine rddump(ird_call)

		use param
		use cotrol
		use domain
		use equil
		use dynamo
!DIR$ DEFINE PLPLOT_WORKING=0
!DIR$ IF(PLPLOT_WORKING .eq. 1)
!                USE plplot
!DIR$ ENDIF
!		use bspline
		implicit none
                include "/Users/7yg/ornldev/Codes/silo-4.10.2/include/silo.inc"

		integer :: i,l,j,ihisto,lmaxo,icycl,icol,ic,mjbz
		real(IDP) :: pertsclo		
		character(len=2), dimension(3) :: numrunp

				
		integer, dimension(ldim) :: lmap
		integer, allocatable, dimension(:) :: color_set
		real*8, allocatable, dimension(:) :: yy,phi_all,rr,temp_all
		real(IDP), dimension(ldim) :: widthio,gammaio
		real(IDP) :: ymin, ymax, scale, dr
            real*8, dimension(:,:), allocatable :: rmnc, zmns, phimns
            real*8, dimension(:,:), allocatable :: rmns, zmnc, phimnc
            integer, dimension(:), allocatable :: mmbz,nnbz
            integer :: icc, lp, itht, ierr, ndims, npts, dbfile, err
            real, dimension(:), allocatable :: x, z, tht_p, phi_plot, rad, xouter, zouter, rderiv
            real, dimension(:), allocatable :: flux_surfs,yp,temp,qqz,qcontour,zonal_z0
	    logical :: i_exist, lasym, Trapped_on
	    real*8 :: twopi, arg, zeta
            real*8, dimension(:), allocatable :: bcoef,xfit,yfit,xknot
            real*8, dimension(:,:), allocatable :: plot_var_intrp
            real*8 :: nw_yfit, xval
            integer :: kxord, ird_call
	    twopi = 8.*atan(1.0)
            Trapped_on = .false.
            if(ird_call .eq. 1) then
		read(8) ihisto,numruno,numrunp,numruns,numeq,numvac,(numhist(i),i=1,ihisto),maxstp,nstep, &
			ndump,nprint,lplots,ndiag,jmn,jmx,jint,itime,dt0,nstep1,nonlin,m0dy,nocpl,ifreezd,ifreezu, &
			Adens,Bdens,LcA0,LcA1,LcA2,LcA3,omcy,dpres,ext_prof,epflr_on,r_epflr,alpha_on,iflr_on,iflr,twofl_on,ieldamp_on, &
			omegar,bet0_alp,Adensalp,Bdensalp,LcA0alp,LcA1alp,LcA2alp,LcA3alp,omcyalp,r_epflralp
		read(8) mj,lmax,leqmax,mjm1
		read(8) (r(j),j=0,mj),(mm(l),l=1,lmax),(nn(l),l=1,lmax),(mmeq(l),l=1,leqmax),(nneq(l),l=1,leqmax),(rinv(j),j=0,mj), &		
			(dc1m(j),j=1,mj),(dc1p(j),j=1,mj),(dc2m(j),j=1,mj),(dc2p(j),j=1,mj),(del2cm(j),j=1,mj),(del2cp(j),j=1,mj)
	
		allocate (sqgi(0:mj,0:leqmax),sqg(0:mj,0:leqmax),bst(0:mj,0:leqmax),grr(0:mj,0:leqmax),grt(0:mj,0:leqmax), &
			  gtt(0:mj,0:leqmax),grroj(0:mj,0:leqmax),grtoj(0:mj,0:leqmax),gttoj(0:mj,0:leqmax),jbgrr(0:mj,0:leqmax), &
			  jbgrt(0:mj,0:leqmax),jbgtt(0:mj,0:leqmax),lplrr(0:mj,0:leqmax),lplrt(0:mj,0:leqmax),lplrz(0:mj,0:leqmax), &
			  lpltt(0:mj,0:leqmax),lpltz(0:mj,0:leqmax),lplzz(0:mj,0:leqmax),lplr(0:mj,0:leqmax),lplt(0:mj,0:leqmax), &
			  lplz(0:mj,0:leqmax),djroj(0:mj,0:leqmax),djtoj(0:mj,0:leqmax),djzoj(0:mj,0:leqmax),omdr(0:mj,0:leqmax), &
			  omdt(0:mj,0:leqmax),omdz(0:mj,0:leqmax),bmod(0:mj,0:leqmax),dbsjtoj(0:mj,0:leqmax),dbsjzoj(0:mj,0:leqmax))
!               write(0,'("nstep = ",i6)') nstep
		
		sqgi=0.0_IDP
		sqg=0.0_IDP
		bst=0.0_IDP
		grr=0.0_IDP
		grt=0.0_IDP
		gtt=0.0_IDP
		grroj=0.0_IDP
		grtoj=0.0_IDP
		gttoj=0.0_IDP
		jbgrr=0.0_IDP
		jbgrt=0.0_IDP
		jbgtt=0.0_IDP
		lplrr=0.0_IDP
		lplrt=0.0_IDP
		lplrz=0.0_IDP
		lpltt=0.0_IDP
		lpltz=0.0_IDP
		lplzz=0.0_IDP
		lplr=0.0_IDP
		lplt=0.0_IDP
		lplz=0.0_IDP
		djroj=0.0_IDP
		djtoj=0.0_IDP
		djzoj=0.0_IDP
		omdr=0.0_IDP
		omdt=0.0_IDP
		omdz=0.0_IDP	
		bmod=0.0_IDP
		dbsjtoj=0.0_IDP
		dbsjzoj=0.0_IDP
		read(8) ni,nis,ne,delta,rc,fti,fte
		read(8) (qq(j),j=0,mj),(qqinv(j),j=0,mj),(psieq(j),j=0,mj),(chieq(j),j=0,mj),(preq(j),j=0,mj),(feq(j),j=0,mj), &
			(cureq(j),j=0,mj),(denseq(j),j=0,mj),(teeq(j),j=0,mj),(nfeq(j),j=0,mj),(vfova(j),j=0,mj),(vzt_eq(j),j=0,mj), &
			(tieq(j),j=0,mj)
		if (alpha_on == 1) read(8) (nalpeq(j),j=0,mj),(valphaova(j),j=0,mj)
		if (ext_prof == 1) then
			allocate (vAlfven(0:mj),vtherm_elcP(0:mj))
			read(8) (vAlfven(j),j=0,mj),(vtherm_elcP(j),j=0,mj)
		else
			read(8) (vtherm_elc(j),j=0,mj)
		end if
		read(8) ngeneq,ndevice(1),ndevice(2),ieq,eps,bet0,bet0_f,nqprof,q0,qlamb,ro,(rs(l),l=1,lmax),leq,qh,rsbar,qlambpp, &
			ndat,(xr(j),j=1,ndat),(xdat(j),j=1,ndat),pop,popp,pol2,ltwo,betaj,(pdat(j),j=1,ndat),ntok,betat,betap
		read(8) ((sqgi(j,l),j=0,mj),l=1,leqmax),((sqg(j,l),j=0,mj),l=1,leqmax),((bst(j,l),j=0,mj),l=1,leqmax), &
			((grr(j,l),j=0,mj),l=1,leqmax),((grt(j,l),j=0,mj),l=1,leqmax),((gtt(j,l),j=0,mj),l=1,leqmax), &
			((grroj(j,l),j=0,mj),l=1,leqmax),((grtoj(j,l),j=0,mj),l=1,leqmax),((gttoj(j,l),j=0,mj),l=1,leqmax), &
			((jbgrr(j,l),j=0,mj),l=1,leqmax),((jbgrt(j,l),j=0,mj),l=1,leqmax),((jbgtt(j,l),j=0,mj),l=1,leqmax), &
			((lplrr(j,l),j=0,mj),l=1,leqmax),((lplrt(j,l),j=0,mj),l=1,leqmax),((lplrz(j,l),j=0,mj),l=1,leqmax), &
			((lpltt(j,l),j=0,mj),l=1,leqmax),((lpltz(j,l),j=0,mj),l=1,leqmax),((lplzz(j,l),j=0,mj),l=1,leqmax), &
			((lplr(j,l),j=0,mj),l=1,leqmax),((lplt(j,l),j=0,mj),l=1,leqmax),((lplz(j,l),j=0,mj),l=1,leqmax), &
			((djroj(j,l),j=0,mj),l=1,leqmax),((djtoj(j,l),j=0,mj),l=1,leqmax),((djzoj(j,l),j=0,mj),l=1,leqmax), &
			((omdr(j,l),j=0,mj),l=1,leqmax),((omdt(j,l),j=0,mj),l=1,leqmax),((omdz(j,l),j=0,mj),l=1,leqmax), &
			((bmod(j,l),j=0,mj),l=1,leqmax),((dbsjtoj(j,l),j=0,mj),l=1,leqmax),((dbsjzoj(j,l),j=0,mj),l=1,leqmax)
		if (ieldamp_on == 1) then
			allocate (eildr(0:mj,0:leqmax),eildt(0:mj,0:leqmax),eildz(0:mj,0:leqmax),eildrr(0:mj,0:leqmax), &
				  eildrt(0:mj,0:leqmax),eildrz(0:mj,0:leqmax),eildtt(0:mj,0:leqmax),eildtz(0:mj,0:leqmax), &
				  eildzz(0:mj,0:leqmax))
			read(8) ((eildr(j,l),j=0,mj),l=1,leqmax),((eildt(j,l),j=0,mj),l=1,leqmax), &
				((eildz(j,l),j=0,mj),l=1,leqmax),((eildrr(j,l),j=0,mj),l=1,leqmax), &
				((eildrt(j,l),j=0,mj),l=1,leqmax),((eildrz(j,l),j=0,mj),l=1,leqmax), &
				((eildtt(j,l),j=0,mj),l=1,leqmax),((eildtz(j,l),j=0,mj),l=1,leqmax), &
				((eildzz(j,l),j=0,mj),l=1,leqmax)
		end if
		
		if(Trapped_on .eqv. .true.) allocate (omdrprp(0:mj,0:leqmax),omdtprp(0:mj,0:leqmax),omdzprp(0:mj,0:leqmax))	

         if(Trapped_on .eqv. .true.) then
		  read(8)  ((omdrprp(j,l),j=0,mj),l=1,leqmax),((omdtprp(j,l),j=0,mj),l=1,leqmax),((omdzprp(j,l),j=0,mj),l=1,leqmax)
		end if	

		read(8) (eta(j),j=0,mj),dt,time
		
	  else if(ird_call .eq. 2) then
	  lmaxo = lmax
          do l = 1,lmaxo
	   lmap(l) = l
	  end do
	  allocate (plot_var(0:jdim,0:ldim))
!	  write(*,*) variable
!	  write(*,*) phi(mj/2,lmaxo/2)
			
!
!    Select field for subsequent plots:
!		
		 do j = 0,mj
		 do l = 1,lmaxo
		 if(variable .eq. 1) then
		   plot_var(j,l) = psi(j,l)
		 else if(variable .eq. 2) then
		   plot_var(j,l) = phi(j,l)
		 else if(variable .eq. 3) then
		   plot_var(j,l) = pr(j,l)
		 else if(variable .eq. 4) then
		   plot_var(j,l) = nf(j,l)
		 else if(variable .eq. 5) then
		   plot_var(j,l) = vprlf(j,l)
		 else if(variable .eq. 6) then
		   plot_var(j,l) = vthprlf(j,l)
		 end if
		 end do
		 end do
!	  write(*,*) plot_var(mj/2,lmaxo/2)
	  
	  if(zonal_plot .gt. 0) then
	  allocate(rderiv(0:mj))
	   do l=1,lmaxo
	    do j=1,mj-1
	     dr = 2./dble(mj)
	     rderiv(j) = (plot_var(j+1,l) - plot_var(j-1,l))/dr
	    end do
	    rderiv(0) = (plot_var(1,l) - plot_var(0,l))/(0.5*dr)
	    rderiv(mj) = (plot_var(mj,l) - plot_var(mj-1,l))/(0.5*dr)
	    do j=0,mj
	     plot_var(j,l) = rderiv(j)
	    end do
	   end do
          end if
	  
!DIR$ IF(PLPLOT_WORKING .eq. 1)		
      allocate(color_set(9))
      allocate(yy(mj))
      allocate(rr(mj))
      allocate(phi_all(mj*lmaxo))
      ic = 0
      do l = 1,lmaxo
       do j=1,mj
        ic = ic + 1
        phi_all(ic) = plot_var(j,lmap(l))
       end do
      end do 
      ymax = maxval(phi_all); ymin = minval(phi_all)
      scale = max(abs(ymax),abs(ymin))
      ymax = 1.1*ymax/scale; ymin = 1.1*ymin/scale
!      write(0,*) ymin, ymax
      do j=1,mj
       rr(j) = r(j)
      end do     
      color_set(1) = 15;color_set(2) = 1;color_set(3) = 3
      color_set(4) = 4;color_set(5) = 8;color_set(6) = 9
      color_set(7) = 10;color_set(8) = 12;color_set(9) = 13
!      call plscol0(0, 255, 255, 255)
!      call plscol0(15, 0, 0, 0)
!      call plsfam(1,1,200000)
!      call plssub(1,1)
!      call plinit
!      call plscolor(1)
!      call plcol0(15)
!      call plfont(2)
!      call plenv(0.d0, 1.d0, ymin, ymax, 0, 0)
!      call plschr(0.d0,0.7d0)
!      call pllab('<r>/<a>','Phi_mn','FAR3D Phi eigenmode')
!      write(0,*) lmaxo
      do l = 1,lmaxo
       if(nn(l) .ne. 0) then
!	if(l .eq.lmaxo/2) write(0,*) mm(l),nn(l)
!	if(l .eq.lmaxo/2) write(0,*) mm(lmap(l)),nn(lmap(l))
       do j=1,mj
        yy(j) = plot_var(j,lmap(l))/scale
!	if(l .eq.lmaxo/2) write(0,*) rr(j),yy(j)
       end do
       icycl = mod(l,8) + 1
       icol = color_set(icycl)
!       call plcol0(icol)
!       call plwidth(3.d0)
!       call plline(rr,yy)
!       call plwidth(1.d0)
!       call plcol0(15)
      end if
      end do
!      call plend
!DIR$ ENDIF
!      Generate Visit 2D plot
!
!      Check to see if geom_bzr_to_cyl file is available, and if so read it in.
!      If not present, then stop.
!	
        i_exist = .false.
	inquire(file='geom_bzr_to_cyl.dat',exist=i_exist)
	if(.not.i_exist) then
	 write(0,'("Need geom_bzr_to_cyl.dat file to be present")')
	 stop 88
	end if
	open(unit=30,file='geom_bzr_to_cyl.dat',status='old')
	read(30,*) mjbz, lbmax, lasym
	 allocate (rmnc(0:mj,lbmax))
	 allocate (zmns(0:mj,lbmax))
	 allocate (phimns(0:mj,lbmax))
	 allocate (qqz(0:mj))
	if(lasym) then
	 allocate (rmns(0:mj,lbmax))
	 allocate (zmnc(0:mj,lbmax))
!	 allocate (phimnc(0:mj,lbmax))
	endif
	allocate (rad(0:mj))
	allocate (mmbz(lbmax))
	allocate (nnbz(lbmax))
	do l=1,lbmax
          read(30,*) mmbz(l),nnbz(l)
	end do
	do j=0,mjbz
	  read(30,*) rad(j), qqz(j)
!	  write(0,*) rad(j), qqz(j)
	end do
	do l=1,lbmax
	 do j=0,mj
!	if(lasym) then
!          read(30,'(e15.8,5(2x,e15.8))') rmnc(l,j),zmns(l,j), phimns(j,l),  &
!     	 rmns(l,j), zmnc(l,j),phimnc(l,j)
!	else
!          read(30,'(e15.8,2(2x,e15.8))') rmnc(l,j),  &
!                      zmns(l,j),phimns(l,j)
!	endif
          read(30,'(e15.8,2(2x,e15.8))') rmnc(j,l), zmns(j,l), phimns(j,l)
	 end do		 
	end do
	close(unit=30)	
	
!
!       Generate flux surfaces in 2D cylindrical geometry and calculate field values
!
!       Normalize
		mjp1 = mj + 1
		 ic = 0
         go to 999
		 allocate(temp_all(mjp1*lmaxo))
		 do j = 1,mj
		 do l = 1,lmaxo
		  ic = ic + 1
		  temp_all(ic) = plot_var(j,l)
		 end do
		 end do
		 
                 ymax = maxval(temp_all); ymin = minval(temp_all)
                 scale = max(abs(ymax),abs(ymin))
		 
		 do j = 1,mj
		 do l = 1,lmaxo
		  plot_var(j,l) = plot_var(j,l)/scale
		 end do
		 end do
 999   continue
 
 
!		write(0,*) mj,mjbz
		itht = 600; nfp = 1
		allocate(xouter(itht), zouter(itht))
		zeta = 0.
		allocate(x(itht*mjp1),z(itht*mjp1),phi_plot(itht*mjp1),flux_surfs(itht*mjp1))
		allocate(qcontour(itht*mjp1))
		allocate(tht_p(itht))
		allocate(zonal_z0(0:mj))
		x(:) = 0.
		z(:) = 0.
		phi_plot(:) = 0.
		icc = 0
		do j = 0,mj
		 do i = 1,itht
		  icc = icc + 1
		  tht_p(i) = twopi*(real(i-1)/real(itht-1))
                  x(icc) = 0.; z(icc) = 0.
		  
		  do l = 1,lbmax
		  
		   if(lasym) then
		    arg = tht_p(i)*real(mmbz(l)) - zeta*real(nnbz(l))
		    x(icc) = x(icc) + rmnc(j,l)*cos(arg) + rmns(j,l)*sin(arg)
		    z(icc) = z(icc) + zmns(j,l)*sin(arg) + zmnc(j,l)*cos(arg)

     		   else
		    arg = tht_p(i)*real(mmbz(l)) - zeta*real(nnbz(l))
		    x(icc) = x(icc) + rmnc(j,l)*cos(arg)
		    z(icc) = z(icc) + zmns(j,l)*sin(arg)
     
                   endif
		   
		  end do
		  if(j .eq. mj) then
		    xouter(i) = x(icc)
		    zouter(i) = z(icc)
		  endif
		  
		 phi_plot(icc) = 0.
		 		   		   
		 if(zonal_plot .eq. 0) then
		 do l = 1,lmaxo
		  arg = real(mm(l))*tht_p(i)-zeta*real(nn(l))
		   if(signl(l) .eq. 1) then
		     phi_plot(icc) = phi_plot(icc) + plot_var(j,lmap(l))*sin(arg)
		    else if(signl(l) .eq. -1) then
		     phi_plot(icc) = phi_plot(icc) + plot_var(j,lmap(l))*cos(arg)
		    endif
		   end do
		 else if(zonal_plot .ne. 0) then
		 do l = 1,lmaxo
		  if(nn(l) .eq. 0) then
		   arg = real(mm(l))*tht_p(i)
 		    if(signl(l) .eq. 1) then
		     phi_plot(icc) = phi_plot(icc) + plot_var(j,lmap(l))*sin(arg)
		    else if(signl(l) .eq. -1) then
		     phi_plot(icc) = phi_plot(icc) + plot_var(j,lmap(l))*cos(arg)
		    endif
		   endif
		  end do
		 end if
!      ************** check is rad flux or sqrt(flux)?
		   flux_surfs(icc) = real(rad(j))/real(rad(mjbz))
		   qcontour(icc) = qqz(j)
		  if(i .eq. 1 .and. zonal_plot .ne. 0) zonal_z0(j) = phi_plot(icc)
		 end do     !do i = 1,itht
		end do      !do j = 0,mj

		if(zonal_plot .ne. 0) then
		 open(unit=87,file='zonal_at_z0.dat',status="unknown")
		  do j=0,mj
		   write(87,'(e15.8,2x,e15.8)') rad(j),zonal_z0(j)
		  end do
		 close(unit=87)
		end if

!    Write data out to Visit Silo data file
!
      if(variable .eq. 1) then
       err = dbcreate("2D_psi.silo", 11, DB_CLOBBER,  &
        DB_LOCAL,"Comment about the data", 22, DB_PDB, dbfile)
      else if(variable .eq. 2) then
       err = dbcreate("2D_phi.silo", 11, DB_CLOBBER,  &
        DB_LOCAL,"Comment about the data", 22, DB_PDB, dbfile)
      else if(variable .eq. 3) then
       err = dbcreate("2D_pressure.silo", 16, DB_CLOBBER,  &
        DB_LOCAL,"Comment about the data", 22, DB_PDB, dbfile)
      else if(variable .eq. 4) then
       err = dbcreate("2D_nfast.silo", 13, DB_CLOBBER,  &
        DB_LOCAL,"Comment about the data", 22, DB_PDB, dbfile)
     else if(variable .eq. 5) then
       err = dbcreate("2D_vprlf.silo", 13, DB_CLOBBER,  &
        DB_LOCAL,"Comment about the data", 22, DB_PDB, dbfile)
      else if(variable .eq. 6) then
       err = dbcreate("2D_vthprl.silo", 14, DB_CLOBBER,  &
        DB_LOCAL,"Comment about the data", 22, DB_PDB, dbfile)
      end if
	if(err .ne. 0) stop 27
        ndims = 2; npts = itht*mjp1
       err = dbputpm(dbfile, "pointmesh",9, ndims, x, z, DB_F77NULL,  &
           npts, DB_FLOAT, DB_F77NULL, ierr)
	if(ierr .ne. 0) stop 28
	
       if(variable .eq. 1) then
        err=dbputpv1(dbfile,"psi",3,"pointmesh",9,phi_plot,npts,DB_FLOAT,DB_F77NULL,ierr)
       else if(variable .eq. 2) then
        err=dbputpv1(dbfile,"phi",3,"pointmesh",9,phi_plot,npts,DB_FLOAT,DB_F77NULL,ierr)
       else if(variable .eq. 3) then
        err=dbputpv1(dbfile,"pres",4,"pointmesh",9,phi_plot,npts,DB_FLOAT,DB_F77NULL,ierr)
       else if(variable .eq. 4) then
        err=dbputpv1(dbfile,"nfast",5,"pointmesh",9,phi_plot,npts,DB_FLOAT,DB_F77NULL,ierr)
       else if(variable .eq. 5) then
        err=dbputpv1(dbfile,"vprl",4,"pointmesh",9,phi_plot,npts,DB_FLOAT,DB_F77NULL,ierr)
       else if(variable .eq. 6) then
        err=dbputpv1(dbfile,"vthprl",6,"pointmesh",9,phi_plot,npts,DB_FLOAT,DB_F77NULL,ierr)
       end if
       
	if(ierr .ne. 0) stop 29
       err = dbputpv1(dbfile, "flux", 4, "pointmesh", 9, flux_surfs,  &
           npts, DB_FLOAT, DB_F77NULL, ierr)
	if(ierr .ne. 0) stop 30
       err = dbputpv1(dbfile, "q", 1, "pointmesh", 9, qcontour,  &
           npts, DB_FLOAT, DB_F77NULL, ierr)
	if(ierr .ne. 0) stop 30
      err = dbclose(dbfile)
      
!       err = dbcreate("Outer_surface.silo", 18, DB_CLOBBER,  &
!        DB_LOCAL,"XZ_outer", 8, DB_PDB, dbfile)
!       err = dbputpm(dbfile, "pointmesh",9, ndims, xouter, zouter, DB_F77NULL,  &
!           itht, DB_FLOAT, DB_F77NULL, ierr)
!      err = dbclose(dbfile)
      
      open(unit=15,file="bound.curve",status="unknown")
      do i=1,itht
       write(15,'(f10.7,2x,f10.7)') xouter(i), zouter(i)
      end do
      close(15)
      
      open(unit=17,file="matlb_2D",status="unknown")
      do i=1,npts
       write(17,'(e15.8,2(2x,e15.8))') x(i), z(i), phi_plot(i)
      end do
      close(unit=17)
      
      end if
	end subroutine rddump

	subroutine setmod

		use param
		use cotrol
		use domain
		use dynamo
		implicit none
		
		integer :: l,n,m,lh,mt,nt,ngcd,ndiv,lll,i,lm,lq,mcnt,nl,lp,lpp,lppn,lop,l1,mband,mmp,nnp,np,nmaxx

		mmin=0
		mmax=0
		nmin=0
		nmax=0
		mmineq=0
		mmaxeq=0
		nmineq=0
		nmaxeq=0
		do l=1,lmax
			m=mm(l)
			n=nn(l)
			signl(l)=1
			if (n < 0 .or. (n == 0 .and. m < 0)) signl(l)=-1
			if (n == 0 .and. m == 0) signl(l)=0
		end do
		mmax=maxval(mm(1:lmax))
		mmin=minval(mm(1:lmax))
		nmax=maxval(nn(1:lmax))
		nmin=minval(nn(1:lmax))
		mmaxx=max(mmax,abs(mmin))		
		do l=1,leqmax
			m=mmeq(l)
			n=nneq(l)
			sgnleq(l)=1
			if (n < 0 .or. (n == 0 .and. m < 0)) sgnleq(l)=-1
			if (n == 0 .and. m == 0) sgnleq(l)=0
		end do
		mmaxeq=maxval(mmeq(1:leqmax))
		mmineq=minval(mmeq(1:leqmax))
		nmaxeq=maxval(nneq(1:leqmax))
		nmineq=minval(nneq(1:leqmax))
		mmaxxeq=max(mmax,abs(mmin))		

		allocate (ll(mmin:mmax,nmin:nmax))

		ll=0
		do l=1,lmax
			ll(mm(l),nn(l))=l
		end do
		l0=0
		if (0 >= mmin .and. 0 <= mmax .and. 0 >= nmin .and. 0 <= nmax) l0=ll(0,0)
		if (l0 == 0) then
!			write (6,'("  setmod: l0=0")')
			stop
		end if

		allocate (lleq(mmineq:mmaxeq,nmineq:nmaxeq))

		lleq=0
		do l=1,leqmax
			lleq(mmeq(l),nneq(l))=l
		end do
		leq0=0
		if (0 >= mmineq .and. 0 <= mmaxeq .and. 0 >= nmineq .and. 0 <= nmaxeq) leq0=lleq(0,0)
		if (leq0 == 0) then
!			write (6,'("  setmod: leq0=0")')
			stop
		end if

!  find prime harmonics.

		lh=0
		mnxxx=1
		do l=1,lmax

			mt=mm(l)*signl(l)
			nt=nn(l)*signl(l)

			if (mt == 0 .or. nt == 0) then
				if (nt /= 0) then
					mnxxx=max(mnxxx,nt)
					nt=1
				end if
				if (mt /= 0) then
					mnxxx=max(mnxxx,mt)
					mt=1
				end if
			else
				ngcd=1
				do ndiv=2,nt
					if(mt /= ndiv*(mt/ndiv)) cycle
					if(nt /= ndiv*(nt/ndiv)) cycle
					ngcd=ndiv
				end do
				mnxxx=max(mnxxx,ngcd)
				mt=mt/ngcd
				nt=nt/ngcd
			end if

			do lll=1,lh
				if (mt == mh(lll) .and. nt == nh(lll)) exit
			end do
			if (lh == 0 .or. lll > lh) then
				lh=lh+1
				mh(lh)=mt
				nh(lh)=nt
			end if
		end do
		lhmax=lh

!		write(6,'(/"helicities"/"   lh   mh   nh")')
!		write(6,'(3i5)') (lh,mh(lh),nh(lh),lh=1,lhmax)
!		write(6,'(/" mnxxx = ",i3)') mnxxx

		lh=0
		do l=1,leqmax

			mt=mmeq(l)*sgnleq(l)
			nt=nneq(l)*sgnleq(l)

			if (mt == 0 .or. nt == 0) then
				if (nt /= 0) nt=1
				if (mt /= 0) mt=1
			else
				ngcd=1
				do ndiv=2,nt
					if(mt /= ndiv*(mt/ndiv)) cycle
					if(nt /= ndiv*(nt/ndiv)) cycle
					ngcd=ndiv
				end do
				mt=mt/ngcd
				nt=nt/ngcd
			end if

			do lll=1,lh
				if (mt == mheq(lll) .and. nt == nheq(lll)) exit
			end do
			if (lh == 0 .or. lll > lh) then
				lh=lh+1
				mheq(lh)=mt
				nheq(lh)=nt
			end if
		end do
		lheqmx=lh

		nmaxx=max(-nmin,nmax)
		allocate (m1n(-nmaxx:nmaxx))
		allocate (mrang(-nmaxx:nmaxx))

		do n=nmin,nmax
			m1n(n)=mmax+1
			mrang(n)=mmin-1
			do l=1,lmax
				if (nn(l) /= n) cycle
				m1n(n)=min(m1n(n),mm(l))
				mrang(n)=max(mrang(n),mm(l))
			end do
			if (mrang(n) == mmin-1) then
				mrang(n)=0
			else
				mrang(n)=mrang(n)-m1n(n)+1
			endif
		end do
		if (nmin < 0 .and. nmax > 0) then
			if (nmin /= -nmax) then
!				write (6,'("  setmod: nmin =",i4," nmax =",i3)') nmin,nmax
				stop
			end if			
			do n=1,nmax
				if (mrang(n)+mrang(-n) > 0 .and. (mrang(-n) /= mrang(n) .or. m1n(n) /= -(m1n(-n)+mrang(-n)-1))) then
!					write (6,'("  setmod: n =",i4," mrang =",i3,i4," m1n =",i3,i5)') n,mrang(n),mrang(-n),m1n(n),m1n(-n)+mrang(-n)-1
					stop
				end if
			end do
		end if

		if (nocpl == 0) then

			lmax0=0
			do i=1,mrang(0)
				m=m1n(0)+i-1
				l=ll(m,0)
				if (l == 0) cycle
				lmax0=lmax0+1
			end do

			allocate (ll0(lmax0))

			lm=0
			do i=1,mrang(0)
				m=m1n(0)+i-1
				l=ll(m,0)
				if (l == 0) cycle
				lm=lm+1
				ll0(lm)=l
			end do
			
		else
		
			lmax0=1
			allocate (ll0(lmax0))
			ll0(1)=l0
		
		end if

		lmaxn=lmax-leqmax
		if (m0dy < 0) m0dy=0
		if (nonlin == 0) m0dy=0
		lmaxn=lmaxn+m0dy
		lmx=noeqn*lmaxn

		allocate (lln(lmaxn))

		nnum=0

		if (nocpl == 0) then

			do n=nmin,nmax
				if (mrang(n) == 0) cycle
				if (n < 0 .and. mrang(-n) /= 0) cycle
				nnum=nnum+1
				mcnt=0
				do i=1,mrang(n)
					m=m1n(n)+i-1
					l=ll(m,n)
					lq=0
					if (n >= nmineq .and. n <= nmaxeq .and. m >= mmineq .and. m <= mmaxeq) lq=lleq(m,n)
					if (l == 0 .or. lq > m0dy) cycle
					mcnt=mcnt+1
				end do
				if (-n >= nmin .and. -n <= nmax .and. n /= 0) then
					do i=1,mrang(-n)
						m=m1n(-n)+i-1
						l=ll(m,-n)
						lq=0
						if (-n >= nmineq .and. -n <= nmaxeq .and. m >= mmineq .and. m <= mmaxeq) lq=lleq(m,-n)
						if (l == 0 .or. lq > m0dy) cycle
						mcnt=mcnt+1
					end do
				end if
				if (mcnt == 0) nnum=nnum-1
			end do

			allocate (mnumn(nnum))
			allocate (lnumn(0:nnum))

			nnd=0
			lnumn(0)=0
			nl=0
			lp=0
			do n=nmin,nmax
				if (mrang(n) == 0) cycle
				if (n < 0 .and. mrang(-n) /= 0) cycle
				nl=nl+1
				lpp=0
				lppn=0
				do i=1,mrang(n)
					m=m1n(n)+i-1
					l=ll(m,n)
					lq=0
					if (n >= nmineq .and. n <= nmaxeq .and. m >= mmineq .and. m <= mmaxeq) lq=lleq(m,n)
					if (l == 0 .or. lq > m0dy) cycle
					lp=lp+1
					lln(lp)=l
					lpp=lpp+1
				end do
				if (-n >= nmin .and. -n <= nmax .and. n /= 0) then
					do i=mrang(-n),1,-1
						m=m1n(-n)+i-1
						l=ll(m,-n)
						lq=0
						if (-n >= nmineq .and. -n <= nmaxeq .and. m >= mmineq .and. m <= mmaxeq) lq=lleq(m,-n)
						if (l == 0 .or. lq > m0dy) cycle
						lp=lp+1
						lln(lp)=l
						lppn=lppn+1
					end do
				end if
				if (lpp > 0) then
					mnumn(nl)=lpp
					lnumn(nl)=lp
					if (nmin == 0 .or. nmax == 0 .or. n == 0) then
						nnd=nnd+1
					else if (lppn /= lpp) then
						write (6,'("  setmod: n =",i4," mnumn =",i3,i4)') n,lpp,lppn
						stop
					end if
				else
					nl=nl-1
				end if
			end do
			nst=nnd+1

!			write (6,'(/"lln="/("    ",25i5))') (lln(l),l=1,lmaxn)

		else

			do n=nmin,nmax
				if (mrang(n) == 0) cycle
				do i=1,mrang(n)
					m=m1n(n)+i-1
					l=ll(m,n)
					if(l == 0) cycle
					lop=0
					if (-m >= mmin .and. -m <= mmax .and. -n >= nmin .and. -n <= nmax) lop=ll(-m,-n)
					if (signl(l) < 0 .and. lop /= 0) cycle
					lq=0
					if (n >= nmineq .and. n <= nmaxeq .and. m >= mmineq .and. m <= mmaxeq) lq=lleq(m,n)
					if (lq > m0dy) cycle
					nnum=nnum+1
					mcnt=1
					if (lop /= 0 .and. (m /= 0 .or. n /= 0)) mcnt=mcnt+1
				end do
			end do

			allocate (mnumn(nnum))
			allocate (lnumn(0:nnum))

			nnd=0
			lnumn(0)=0
			nl=0
			lp=0
			do n=nmin,nmax
				if (mrang(n) == 0) cycle
				do i=1,mrang(n)
					m=m1n(n)+i-1
					l=ll(m,n)
					if (l == 0) cycle
					lop=0
					if (-m >= mmin .and. -m <= mmax .and. -n >= nmin .and. -n <= nmax) lop=ll(-m,-n)
					if (signl(l) < 0 .and. lop /= 0) cycle
					lq=0
					if (n >= nmineq .and. n <= nmaxeq .and. m >= mmineq .and. m <= mmaxeq) lq=lleq(m,n)
					if (lq > m0dy) cycle
					nl=nl+1
					lp=lp+1
					lpp=1
					lln(lp)=l
					if (lop /= 0 .and. (m /= 0 .or. n /= 0)) then
						lp=lp+1
						lln(lp)=lop
					else
						nnd=nnd+1
					end if
					mnumn(nl)=lpp
					lnumn(nl)=lp
				end do
			end do
			nst=nnd+1

		end if

		allocate (lo(lmaxn))

		lo=0
		do l=1,lmaxn
			m=-mm(lln(l))
			n=-nn(lln(l))
			if (m < mmin .or. m > mmax .or. n < nmin .or. n > nmax) cycle
			do l1=1,lmaxn
				if (mm(lln(l1)) == m .and. nn(lln(l1)) == n) exit
			end do
			if (l1 > lmaxn) then
				lo(l)=0
			else
				lo(l)=l1
			end if
		end do

		mband=0
		do m=0,mmax
			if (ll(m,0) /= 0) mband=mband+1
		end do
		mxmband=2*mband-1
		do n=1,nmax
			mband=0
			do m=mmin,mmax
				if (ll(m,n) /= 0) mband=mband+1
			end do
			if (mband > mxmband) mxmband=mband
		end do

		mband=0
		do m=0,mmaxeq
			if (lleq(m,0) /= 0) mband=mband+1
		end do
		mbandeq=2*mband-1
		do n=1,nmaxeq
			mband=0
			do m=mmineq,mmaxeq
				if (lleq(m,n) /= 0) mband=mband+1
			end do
			if (mband > mbandeq) mbandeq=mband
		end do

	end subroutine setmod	


	subroutine mmlims

	!   Calculate the minimum and maximum m values for each n value
	!   in the complex exponential representation.

		use param
		use domain
		implicit none

		integer :: n,l

		allocate (mmstart(-nmax:nmax))
		allocate (mmend(-nmax:nmax))
		allocate (mmstreq(-nmaxeq:nmaxeq))
		allocate (mmendeq(-nmaxeq:nmaxeq))

		do n = 0,nmax
			mmstart(n) = 1000
			mmend(n) = -1000
		end do

		do l = 1,lmax

			n = nn(l)
			if (n >= 0) then
				if (mm(l) < mmstart(n)) mmstart(n) = mm(l)
				if (mm(l) > mmend(n)) mmend(n) = mm(l)
			else
				if (-mm(l) < mmstart(-n)) mmstart(-n) = -mm(l)
				if (-mm(l) > mmend(-n)) mmend(-n) = -mm(l)
			end if

		end do

		!   in the complex exponential format, [fr,fi](-m,-n) = [fr,-fi](m,n),
		!   so limits for negative n rows are just negatives of corresponding
		!   positive n limits.

		do n = 0,nmax

			mmstart(-n) = -mmend(n)
			mmend(-n) = -mmstart(n)

		end do

		do n = 0,nmaxeq
			mmstreq(n) = 1000
			mmendeq(n) = -1000
		end do

		do l = 1,leqmax

			n = nneq(l)
			if (n >= 0) then
				if (mmeq(l) < mmstreq(n)) mmstreq(n) = mmeq(l)
				if (mmeq(l) > mmendeq(n)) mmendeq(n) = mmeq(l)
			else
				if (-mmeq(l) < mmstreq(-n)) mmstreq(-n) = -mmeq(l)
				if (-mmeq(l) > mmendeq(-n)) mmendeq(-n) = -mmeq(l)
			end if

		end do

		!   in the complex exponential format, [fr,fi](-m,-n) = [fr,-fi](m,n),
		!   so limits for negative n rows are just negatives of corresponding
		!   positive n limits.

		do n = 0,nmaxeq

			mmstreq(-n) = -mmendeq(n)
			mmendeq(-n) = -mmstreq(n)

		end do

	end subroutine mmlims

