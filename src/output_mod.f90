MODULE output_mod

  use param
  use cotrol
  use domain
  use equil
  use dynamo
  use dbyd
  use scratch
  use adios_io_mod

  IMPLICIT NONE

  ! Appended-dumps dataset of this series; stored in every restart file.
  character(len=15), save :: dumps_file = ' '

CONTAINS

  subroutine output

    implicit none

    integer :: i,nd,l,m,n,lh,lheq,le,j

    write(6,'(//52("*"),2a8,51("*")/)') ndevice
    if (nocpl == 0) then
       write(6,'(50("*")," toroidal geometry ",50("*")/)')
    else
       write(6,'(49("*")," cylindrical geometry ",48("*")/)')
    end if
    if (nonlin == 0) write(6,'(56("*")," linear ",55("*")/)')
    if (nonlin /= 0) write(6,'(54("*")," nonlinear ",54("*")//)')

    write(6,'(/"control:"/"numrun  =     ",2a2,a1," numruno =     ",2a2,a1," numruns =     ",2a2,a1, &  
         " nstep   =",i10," maxstp  =",i10," nonlin  =",i10/"ndump   =",i10," nprint  =",i10," ndiag   =",i10, &
         " lplots  =",i10," itime   =",i10," dt0     =",1pe10.3/)') numrun,numruno,numruns,nstep,maxstp,nonlin, &
         ndump,nprint,ndiag,lplots,itime,dt0

    write(6,'("numhist =",12("   ",a6))') (numhist(i),i=1,ihist)

    if (Auto_grid_on == 0) then
       write(6,'(/"domain:"/"mj      =",i10," ni      =",i10," nis     =",i10," ne      =",i10," rc      =",0pf10.7, &
            " delta   =",0pf10.7/"lmax    =",i10," leqmax  =",i10," mmin    =",i10," mmax    =",i10, &
            " nmin    =",i10," nmax    =",i10/"mmineq  =",i10," mmaxeq  =",i10," nmineq  =",i10," nmaxeq  =",i10, &
            " l0      =",i10," leq0    =",i10)') &
            mj,ni,nis,ne,rc,delta,lmax,leqmax,mmin,mmax,nmin,nmax,mmineq,mmaxeq,nmineq,nmaxeq,l0,leq0
    else
       write(6,'(/"domain:"/"mj      =",i10,"  grid with evenly spaced points" &
            /"lmax    =",i10," leqmax  =",i10," mmin    =",i10," mmax    =",i10, &
            " nmin    =",i10," nmax    =",i10/"mmineq  =",i10," mmaxeq  =",i10," nmineq  =",i10," nmaxeq  =",i10, &
            " l0      =",i10," leq0    =",i10)') &
            mj,lmax,leqmax,mmin,mmax,nmin,nmax,mmineq,mmaxeq,nmineq,nmaxeq,l0,leq0
    end if

    write(6,'(/"equil:")')
    if (ext_prof == 0) then
       write(6,'(a)') "eq name:  "//trim(eq_name)
    else
       write(6,'(a)') "eq name:  "//trim(eq_name)//"   ext prof name:  "//trim(ext_prof_name)
    end if
    if (alpha_on == 0) then
       write(6,'("ext_prof        =",i2," alpha_on        =",i2," trapped_on      =",i2," EP_dens_on      =",i2)') &
            ext_prof,alpha_on,trapped_on,EP_dens_on
       write(6,'("R0      =",0pf10.6," B0      =",0pf10.6," eps     =",0pf10.6," bet0    =",1pe10.3," bet0_f  =",1pe10.3, &
            " omcy    =",1pe10.3)') bigrn,bmodn,eps,bet0,bet0_f,omcy
       if (trapped_on == 1) write(6,'("omcyb   =",1pe10.3," rbound  =",1pe10.3)') omcyb,rbound
       if (ext_prof == 0) then
          if (EP_dens_on == 1) then
             write(6,'("Adens   =",1pe10.3," Bdens   =",1pe10.3)') Adens,Bdens
          else
             if (cnfp(0) > 0.0_IDP) write(6,'("cnfp            =",1p6e17.9)') (cnfp(i),i=0,10)
          end if
          if (cnep(0) > 0.0_IDP) write(6,'("cnep            =",1p6e17.9)') (cnep(i),i=0,10)
          if (ctep(0) > 0.0_IDP) write(6,'("ctep            =",1p6e17.9)') (ctep(i),i=0,10)
          if (cvep(0) > 0.0_IDP) write(6,'("cvep            =",1p6e17.9)') (cvep(i),i=0,10)
          if (cvfp(0) > 0.0_IDP) write(6,'("cvfp            =",1p6e17.9)') (cvfp(i),i=0,10)
          if (eqvt(0) > 0.0_IDP) write(6,'("eqvt            =",1p6e17.9)') (eqvt(i),i=0,10)
          if (eqvp(0) > 0.0_IDP) write(6,'("eqvp            =",1p6e17.9)') (eqvp(i),i=0,10)
       else
          write(6,'("DIIID_u         =",i2," q_prof_on       =",i2," EP_vel_on       =",i2," Alpha_vel_on    =",i2, &
               " Eq_presseq_on   =",i2," Eq_presstot_on  =",i2/"Eq_vel_on       =",i2," Eq_velp_on      =",i2, &
               " spe1            =",i2," spe2            =",i2)') DIIID_u,q_prof_on,EP_vel_on,Alpha_vel_on, &
               Eq_presseq_on,Eq_presstot_on,Eq_vel_on,Eq_velp_on,spe1,spe2
       end if
    else
       write(6,'("ext_prof        =",i2," alpha_on        =",i2," trapped_on      =",i2," EP_dens_on      =",i2, &
            " Alpha_dens_on   =",i2)') ext_prof,alpha_on,trapped_on,EP_dens_on,Alpha_dens_on
       if (trapped_on == 0) then
          write(6,'("R0      =",0pf10.6," B0      =",0pf10.6," eps     =",0pf10.6," bet0    =",1pe10.3," bet0_f  =", &
               1pe10.3," bet0_alp=",1pe10.3/"omcy    =",1pe10.3," omcyalp =",1pe10.3)') bigrn,bmodn,eps,bet0, &
               bet0_f,bet0_alp,omcy,omcyalp
       else
          write(6,'("R0      =",0pf10.6," B0      =",0pf10.6," eps     =",0pf10.6," bet0    =",1pe10.3," bet0_f  =", &
               1pe10.3," bet0_alp=",1pe10.3/"omcy    =",1pe10.3," omcyalp =",1pe10.3," omcyb   =",1pe10.3, &
               " rbound  =",1pe10.3)') bigrn,bmodn,eps,bet0,bet0_f,bet0_alp,omcy,omcyalp,omcyb,rbound
       end if
       if (ext_prof == 0) then
          if (EP_dens_on == 1) then
             if (Alpha_dens_on == 1) then
                write(6,'("Adens   =",1pe10.3," Bdens   =",1pe10.3," Adensalp=",1pe10.3," Bdensalp=",1pe10.3)') &
                     Adens,Bdens,Adensalp,Bdensalp
             else
                write(6,'("Adens   =",1pe10.3," Bdens   =",1pe10.3)') Adens,Bdens
                if (cnfpalp(0) > 0.0_IDP) write(6,'("cnfpalp         =",1p6e17.9)') (cnfpalp(i),i=0,10)
             end if
          else
             if (Alpha_dens_on == 1) then
                write(6,'("Adensalp=",1pe10.3," Bdensalp=",1pe10.3)') Adensalp,Bdensalp
                if (cnfp(0) > 0.0_IDP) write(6,'("cnfp            =",1p6e17.9)') (cnfp(i),i=0,10)
             else
                if (cnfp(0) > 0.0_IDP) write(6,'("cnfp            =",1p6e17.9)') (cnfp(i),i=0,10)
                if (cnfpalp(0) > 0.0_IDP) write(6,'("cnfpalp         =",1p6e17.9)') (cnfpalp(i),i=0,10)
             end if
          end if
          if (cnep(0) > 0.0_IDP) write(6,'("cnep            =",1p6e17.9)') (cnep(i),i=0,10)
          if (ctep(0) > 0.0_IDP) write(6,'("ctep            =",1p6e17.9)') (ctep(i),i=0,10)
          if (cvep(0) > 0.0_IDP) write(6,'("cvep            =",1p6e17.9)') (cvep(i),i=0,10)
          if (cvfp(0) > 0.0_IDP) write(6,'("cvfp            =",1p6e17.9)') (cvfp(i),i=0,10)
          if (cvfpalp(0) > 0.0_IDP) write(6,'("cvfpalp         =",1p6e17.9)') (cvfpalp(i),i=0,10)
          if (eqvt(0) > 0.0_IDP) write(6,'("eqvt            =",1p6e17.9)') (eqvt(i),i=0,10)
          if (eqvp(0) > 0.0_IDP) write(6,'("eqvp            =",1p6e17.9)') (eqvp(i),i=0,10)
       else
          write(6,'("DIIID_u         =",i2," q_prof_on       =",i2," EP_vel_on       =",i2, &
               " Eq_presseq_on   =",i2," Eq_presstot_on  =",i2/"Eq_vel_on       =",i2," Eq_velp_on      =",i2, &
               " spe1            =",i2," spe2            =",i2)') DIIID_u,q_prof_on,EP_vel_on, &
               Eq_presseq_on,Eq_presstot_on,Eq_vel_on,Eq_velp_on,spe1,spe2
       end if
    end if
    write(6,'("Edge_on =",i10," edge_p  =",i10)') Edge_on,edge_p

    write(6,'(/"dynamo:"/"s       =",1pe10.3," ietaeq  =",i10," etascl  =",1pe10.3," eta0    =",1pe10.3, &
         " reta    =",0pf10.7," etalmb  =",1pe10.3/"gamma   =",1pe10.3," LcA0    =",1pe10.3, &
         " LcA1    =",1pe10.3," LcA2    =",1pe10.3," LcA3    =",1pe10.3," omegar  =",1pe10.3/"epflr_on=",i10, &
         " r_epflr =",1pe10.3," iflr_on =",i10," iflr    =",1pe10.3," twofl_on=",i10," dpres   =",1pe10.3/ &
         "B_par_on=",i10)') s,ietaeq,etascl,eta0,reta,etalmb,gamma,LcA0,LcA1,LcA2,LcA3,omegar,epflr_on,r_epflr, &
         iflr_on,iflr,twofl_on,dpres,B_par_on
    if (alpha_on == 1) write(6,'("LcA0alp =",1pe10.3," LcA1alp =",1pe10.3," LcA2alp =",1pe10.3," LcA3alp =",1pe10.3, &
         " r_flralp=",1pe10.3)') LcA0alp,LcA1alp,LcA2alp,LcA3alp,r_epflralp
    if (ext_prof == 1) write(6,'("ieldamp_on =",i7," uion    =",1pe10.3," xnuelc0 =",1pe10.3)') ieldamp_on,uion,xnuelc0
    write(6,'("m0dy    =",i10," ipert   =",i10," pertscl =",1pe10.3," xle     =",1pe10.3," dt      =",1pe10.3, &
         " time    =",1pe10.3/"difnr_on=",i10," stdifp  =",1pe10.3," stdifu  =",1pe10.3," stdifv  =",1pe10.3, &
         " stdifnf =",1pe10.3," stdifvf =",1pe10.3)') m0dy,ipert,pertscl,xle,dt,time,difnr_on,stdifp,stdifu, &
         stdifv,stdifnf,stdifvf
    if (alpha_on == 1) write(6,'("difnalp =",1pe10.3," difvalp =",1pe10.3)') stdifnalp,stdifvalp  
    write(6,'("nopsievol_on    =",i2," noprevol_on     =",i2," nonfevol_on     =",i2," nonalpevol_on   =",i2/ &
         "src_sink_th_on  =",i2," src_sink_EP1_on =",i2," src_sink_EP2_on =",i2," src_sink_DIIID  =",i2, &
         " src_sink_ITER   =",i2)') nopsievol_on,noprevol_on,nonfevol_on,nonalpevol_on,src_sink_th_on, &
         src_sink_EP1_on,src_sink_EP2_on,src_sink_DIIID_on,src_sink_ITER_on
    write(6,'("asrc    =",1pe10.3," rsrc    =",1pe10.3," wsrc    =",1pe10.3," asrc_EP1=",1pe10.3," rsrc_EP1=",1pe10.3, &
         " wsrc_EP1=",1pe10.3)') asrc,rsrc,wsrc,asrc_EP1,rsrc_EP1,wsrc_EP1
    if (alpha_on == 1) write(6,'("asrc_EP2=",1pe10.3," rsrc_EP2=",1pe10.3," wsrc_EP2=",1pe10.3)') asrc_EP2,rsrc_EP2, &
         wsrc_EP2
    if (srcsinkth(0) > 0.0_IDP) write(6,'("srcsinkth       =",1p6e17.9)') (srcsinkth(i),i=0,10)
    if (srcsinkEP1(0) > 0.0_IDP) write(6,'("srcsinkEP1      =",1p6e17.9)') (srcsinkEP1(i),i=0,10)
    if (srcsinkEP2(0) > 0.0_IDP) write(6,'("srcsinkEP2      =",1p6e17.9)') (srcsinkEP2(i),i=0,10)

    if (betath_factor /= 1.0_IDP) write(6,'(" WARNING THERMAL BETA FACTOR ACTIVE = ",1pe13.6)') betath_factor 
    if (deltaq /= 0) write(6,'(" WARNING SAFETY FACTOR PROFILE DISPLACEMET ACTIVE = ",1pe13.6)') deltaq
    if (deltaiota /= 0) write(6,'(" WARNING IOTA PROFILE DISPLACEMET ACTIVE = ",1pe13.6)') deltaiota

    write(6,'(/"ll(m,n)")')
    do m=mmax,mmin,-1
       write(6,'(i4,"  ",31i4)') m,(ll(m,n),n=nmin,nmax)
    end do
    write(6,'("   m/n",31i4)') (n,n=nmin,nmax)

    write(6,'(/"helicities"/"   lh   mh   nh")')
    write(6,'(3i5)') (lh,mh(lh),nh(lh),lh=1,lhmax)

    write(6,'(/"    l   mm   nn          signl             rs         widthi         gammai")')
    write(6,'(3i5,"     ",i10,"     ",0pf10.5,1p2e15.3)') (l,mm(l),nn(l),signl(l),rs(l),widthi(l),gammai(l),l=1,lmax)

    write(6,'(/"lleq(m,n)")')
    do m=mmaxeq,mmineq,-1
       write(6,'(i3,"    ",37i3)') m,(lleq(m,n),n=nmineq,nmaxeq)
    end do
    write(6,'("meq/neq",37i3)') (n,n=nmineq,nmaxeq)

    write(6,'(/"eq helicities"/" lheq mheq nheq")')
    write(6,'(3i5)') (lheq,mheq(lheq),nheq(lheq),lheq=1,lheqmx)

    write(6,'(/"  leq mmeq nneq         sgnleq")')
    write(6,'(3i5,"     ",i10)') (le,mmeq(le),nneq(le),sgnleq(le),le=1,leqmax)

    write(6,'(/"  linear matrix mode entries: lmaxn,lln-",i5/("                                        ",12i5))') lmaxn, &
         (lln(i),i=1,lmaxn)

    if (alpha_on == 0) then
       write(6,'(/"           r            q            J            I         preq       denseq         teeq", &
            "         nfeq        vfova          eta")')
       do j=0,mj
          write(6,'(0pf12.8,1p9e13.5)') r(j),qq(j),feq(j),cureq(j),preq(j),denseq(j),teeq(j),nfeq(j),vfova(j),eta(j)
       end do
    else
       write(6,'(/"           r            q            J            I         preq       denseq         teeq", &
            "         nfeq        vfova       nalpeq    valphaova          eta")')
       do j=0,mj
          write(6,'(0pf12.8,1p11e13.5)') r(j),qq(j),feq(j),cureq(j),preq(j),denseq(j),teeq(j),nfeq(j),vfova(j), &
               nalpeq(j),valphaova(j),eta(j)
       end do
    end if

  end subroutine output

  subroutine endrun

    use processor
    use var_para
    use dbyd
    use mult_mod
    use transfer

    implicit none

    integer :: i,j,l,lwrt,l2t,mnum
    real(IDP) :: scmn,scmx,sclmn,sclmx
    real(IDP), dimension(:), allocatable :: scnorm
    character(len=1) :: tb
    character(len=12) :: confil
    character(len=32) :: formatt='("r",1200(a1,i4,"/",i4))'
    character(len=32) :: formatv='(1pe13.6,1200(a1,1pe15.8))'

    interface
       subroutine delstar(ss,ff,itypf,wk1,wk2,wkeq1,c1,c2)
         use param
         use var_para
         implicit none
         integer :: itypf
         real(IDP) :: c1,c2
         real(IDP), dimension(mj_start:,0:) :: ss,ff,wk1,wk2
         real(IDP), dimension(0:,0:) :: wkeq1
       end subroutine delstar
    end interface

    tb=char(9)

    !  store uzt-values into sc4

    do l=1,lmax
       sc4(:,l)=uzt(mj_start:mj_end,l)
    end do

    allocate (scnorm(nnum))
    scnorm=1.0_IDP

    if (myPE == 0) then

       !  vr up
       call dbydth(uzt,phi,-1,0.0_IDP,-eps,0)

       do i=1,nnum
          mnum=mnumn(i)
          if (nonlin == 0) then
             scmn=0.
             scmx=0.
             do l2t=1,mnum
                l=l2t+lnumn(i-1)
                sclmn=minval(uzt(:,lln(l)))
                sclmx=maxval(uzt(:,lln(l)))
                scmn=min(scmn,sclmn)
                scmx=max(scmx,sclmx)
             end do
             if (scmx > abs(scmn)) then
                scnorm(i)=scmx
             else
                scnorm(i)=scmn
             endif
          end if
          if(scnorm(i) /= 0.0_IDP) then
             do l2t=1,mnum
                l=l2t+lnumn(i-1)
                psi(:,lln(l))=psi(:,lln(l))/scnorm(i)
                where (abs(psi(:,lln(l))) < 1.e-50_IDP) psi(:,lln(l))=0
                phi(:,lln(l))=phi(:,lln(l))/scnorm(i)
                where (abs(phi(:,lln(l))) < 1.e-50_IDP) phi(:,lln(l))=0
                pr(:,lln(l))=pr(:,lln(l))/scnorm(i)
                where (abs(pr(:,lln(l))) < 1.e-50_IDP) pr(:,lln(l))=0
                nf(:,lln(l))=nf(:,lln(l))/scnorm(i)
                where (abs(nf(:,lln(l))) < 1.e-50_IDP) nf(:,lln(l))=0
                vprlf(:,lln(l))=vprlf(:,lln(l))/scnorm(i)
                where (abs(vprlf(:,lln(l))) < 1.e-50_IDP) vprlf(:,lln(l))=0
                vthprlf(:,lln(l))=vthprlf(:,lln(l))/scnorm(i)
                where (abs(vthprlf(:,lln(l))) < 1.e-50_IDP) vthprlf(:,lln(l))=0
                uzt(:,lln(l))=uzt(:,lln(l))/scnorm(i)
                where (abs(uzt(:,lln(l))) < 1.e-50_IDP) uzt(:,lln(l))=0
             end do
             if (alpha_on == 1) then
                do l2t=1,mnum
                   l=l2t+lnumn(i-1)
                   nalp(:,lln(l))=nalp(:,lln(l))/scnorm(i)
                   where (abs(nalp(:,lln(l))) < 1.e-50_IDP) nalp(:,lln(l))=0
                   vprlalp(:,lln(l))=vprlalp(:,lln(l))/scnorm(i)
                   where (abs(vprlalp(:,lln(l))) < 1.e-50_IDP) vprlalp(:,lln(l))=0
                end do
             end if
          end if
       end do
       lwrt=abs(lplots)
       if (lwrt > lmax) lwrt=lmax
       ! One field_snapshots.bp step per run: every field is (radial j=0..mj) x
       ! (mode l=1..lwrt), labelled by mode_m/mode_n; r is the radial grid.
       call adios_series_begin(ser_snap)
       call ser_put_r(ser_snap,"time",time)
       call ser_put_i(ser_snap,"nstep",nstep)
       call ser_put_r1(ser_snap,"r",r(0:mj))
       call ser_put_i1(ser_snap,"mode_m",mm(1:lwrt))
       call ser_put_i1(ser_snap,"mode_n",nn(1:lwrt))
       call ser_put_r2(ser_snap,"phi",phi(0:mj,1:lwrt))
       call ser_put_r2(ser_snap,"psi",psi(0:mj,1:lwrt))
       call ser_put_r2(ser_snap,"pr",pr(0:mj,1:lwrt))
       call ser_put_r2(ser_snap,"vr",uzt(0:mj,1:lwrt))
       call ser_put_r2(ser_snap,"nf",nf(0:mj,1:lwrt))
       call ser_put_r2(ser_snap,"vprlf",vprlf(0:mj,1:lwrt))
       call ser_put_r2(ser_snap,"vthprlf",vthprlf(0:mj,1:lwrt))
       if (alpha_on == 1) then
          call ser_put_r2(ser_snap,"nalp",nalp(0:mj,1:lwrt))
          call ser_put_r2(ser_snap,"vprlalp",vprlalp(0:mj,1:lwrt))
       end if

       !  vth up
       call dbydr(uzt,phi,0.0_IDP,eps,0)
       call ser_put_r2(ser_snap,"vth",uzt(0:mj,1:lwrt))
    end if

    !  br up
    call dbydth_par(sc3,psi,1,0.0_IDP,-eps,0)
    !  bth up
    call dbydr_par(sc5,psi,0.0_IDP,eps,0)

    call multed(uzt,sqgi,1,sc3,-1,0.0_IDP,1.0_IDP)
    call trnsfr0(uzt,-1)
    if (myPE == 0) then
       call ser_put_r2(ser_snap,"br",uzt(0:mj,1:lwrt))
    end if

    call multed(uzt,sqgi,1,sc5,1,0.0_IDP,1.0_IDP)
    call trnsfr0(uzt,1)
    if (myPE == 0) then
       call ser_put_r2(ser_snap,"bth",uzt(0:mj,1:lwrt))
    end if

    call delstar(uzt,psi,1,sc2,sc3,sceq1,0.0_IDP,1.0_IDP)
    uzt=eps*uzt
    call trnsfr0(uzt,1)
    if (myPE == 0) then
       call ser_put_r2(ser_snap,"curzt",uzt(0:mj,1:lwrt))
    end if

    !  restore uzt-values

    do l=1,lmax
       uzt(mj_start:mj_end,l)=sc4(:,l)
    end do
    call trnsfr0(uzt,1)
    if (myPE == 0) then
       do i=1,nnum
          mnum=mnumn(i)
          if (nonlin == 0) then
             scmn=0.
             scmx=0.
             do l2t=1,mnum
                l=l2t+lnumn(i-1)
                sclmn=minval(uzt(:,lln(l)))
                sclmx=maxval(uzt(:,lln(l)))
                scmn=min(scmn,sclmn)
                scmx=max(scmx,sclmx)
             end do
             if (scmx > abs(scmn)) then
                scnorm(i)=scmx
             else
                scnorm(i)=scmn
             endif
          end if
          if(scnorm(i) /= 0.0_IDP) then
             do l2t=1,mnum
                l=l2t+lnumn(i-1)
                uzt(:,lln(l))=uzt(:,lln(l))/scnorm(i)
                where (abs(uzt(:,lln(l))) < 1.e-50_IDP) uzt(:,lln(l))=0
             end do
          end if
       end do
       call ser_put_r2(ser_snap,"uzt",uzt(0:mj,1:lwrt))
       call adios_series_end(ser_snap)
    end if

  end subroutine endrun

  subroutine rddump

    ! Phase 1 of the restart read: header, grid/mode arrays, equilibrium and
    ! geometry arrays, and eta/dt/time. Leaves the ADIOS2 engine open -
    ! rddump_flds (called later from resume, once mj_start/mj_end/lmax-sized
    ! arrays are allocated) reads the rest (dynamical fields, trailing
    ! scalars/flags) from the same still-open engine and closes it.
    !
    ! Only understands ADIOS2 restart datasets written by the current
    ! wrdump - the pre-ADIOS2 raw-binary layout (and its old_rd variant) is
    ! no longer read.

    implicit none

    character(len=2), dimension(3) :: numrunp
    integer :: lcnt, ihisto

    if (old_rd) write (0,'(" WARNING: old_rd is set but is no longer honored - ", &
         "restart reads only understand ADIOS2 .bp datasets")')

    call adios_open_read("fs"//numruno(1)//numruno(2)//trim(numruno(3))//".bp")

    ! ihist itself is NOT re-read here: the caller (far3d.f90, before
    ! allocating numhist) already peeked ihist from this same file and set
    ! the global ihist = (file's ihist) + 1 for this run; ihisto below is
    ! just a local re-read of that same file field, used to size numhist.
    call adios_get_i("ihist", ihisto)
    call adios_get_sarr("numrun", numruno)
    call adios_get_sarr("numruno", numrunp)
    call adios_get_sarr("numruns", numruns)
    ! Name of the appended-dumps series this run continues (absent in restart
    ! files written before the series existed: wrdump then derives it from numruns).
    if (adios_has_var("dumps_file")) call adios_get_s("dumps_file", dumps_file)
    call adios_get_sarr("numhist", numhist(1:ihisto))
    call adios_get_i("maxstp", maxstp)
    call adios_get_i("nstep", nstep)
    call adios_get_i("ndump", ndump)
    call adios_get_i("nprint", nprint)
    call adios_get_i("lplots", lplots)
    call adios_get_i("itime", itime)
    call adios_get_r("dt0", dt0)
    call adios_get_i("nstep1", nstep1)
    call adios_get_i("nonlin", nonlin)
    call adios_get_i("m0dy", m0dy)
    call adios_get_i("nocpl", nocpl)
    call adios_get_i("noprevol_on", noprevol_on)
    call adios_get_r("Adens", Adens)
    call adios_get_r("Bdens", Bdens)
    call adios_get_r("LcA0", LcA0)
    call adios_get_r("LcA1", LcA1)
    call adios_get_r("LcA2", LcA2)
    call adios_get_r("LcA3", LcA3)
    call adios_get_r("omcy", omcy)
    call adios_get_r("dpres", dpres)
    call adios_get_i("ext_prof", ext_prof)
    call adios_get_i("epflr_on", epflr_on)
    call adios_get_r("r_epflr", r_epflr)
    call adios_get_i("alpha_on", alpha_on)
    call adios_get_i("iflr_on", iflr_on)
    call adios_get_r("iflr", iflr)
    call adios_get_i("twofl_on", twofl_on)
    call adios_get_i("ieldamp_on", ieldamp_on)
    call adios_get_r("omegar", omegar)
    call adios_get_r("bet0_alp", bet0_alp)
    call adios_get_r("Adensalp", Adensalp)
    call adios_get_r("Bdensalp", Bdensalp)
    call adios_get_r("LcA0alp", LcA0alp)
    call adios_get_r("LcA1alp", LcA1alp)
    call adios_get_r("LcA2alp", LcA2alp)
    call adios_get_r("LcA3alp", LcA3alp)
    call adios_get_r("omcyalp", omcyalp)
    call adios_get_r("r_epflralp", r_epflralp)
    call adios_get_s("ext_prof_name", ext_prof_name)
    call adios_get_i("trapped_on", trapped_on)
    call adios_get_r("omcyb", omcyb)
    call adios_get_r("rbound", rbound)
    call adios_get_i("B_par_on", B_par_on)

    call adios_get_i("mj", mj)
    call adios_get_i("lmax", lmaxo)
    call adios_get_i("leqmax", leqmax)
    call adios_get_i("mjm1", mjm1)

    ! mm/nn (mode numbers) only exist up to lmaxo in the file; when the new
    ! run asks for fewer modes (lmax < lmaxo) only the first lmax are kept
    lcnt = min(lmax,lmaxo)
    call adios_get_r1("r", r)
    call adios_get_i1("mm", mm(1:lcnt))
    call adios_get_i1("nn", nn(1:lcnt))
    call adios_get_i1("mmeq", mmeq)
    call adios_get_i1("nneq", nneq)
    call adios_get_r1("rinv", rinv)
    call adios_get_r1("dc1m", dc1m)
    call adios_get_r1("dc1p", dc1p)
    call adios_get_r1("dc2m", dc2m)
    call adios_get_r1("dc2p", dc2p)
    call adios_get_r1("del2cm", del2cm)
    call adios_get_r1("del2cp", del2cp)

    allocate (sqgi(0:mj,0:leqmax),sqg(0:mj,0:leqmax),bst(0:mj,0:leqmax),grr(0:mj,0:leqmax),grt(0:mj,0:leqmax), &
         gtt(0:mj,0:leqmax),grroj(0:mj,0:leqmax),grtoj(0:mj,0:leqmax),gttoj(0:mj,0:leqmax),jbgrr(0:mj,0:leqmax), &
         jbgrt(0:mj,0:leqmax),jbgtt(0:mj,0:leqmax),lplrr(0:mj,0:leqmax),lplrt(0:mj,0:leqmax),lplrz(0:mj,0:leqmax), &
         lpltt(0:mj,0:leqmax),lpltz(0:mj,0:leqmax),lplzz(0:mj,0:leqmax),lplr(0:mj,0:leqmax),lplt(0:mj,0:leqmax), &
         lplz(0:mj,0:leqmax),djroj(0:mj,0:leqmax),djtoj(0:mj,0:leqmax),djzoj(0:mj,0:leqmax),omdr(0:mj,0:leqmax), &
         omdt(0:mj,0:leqmax),omdz(0:mj,0:leqmax),bmod(0:mj,0:leqmax),dbsjtoj(0:mj,0:leqmax),dbsjzoj(0:mj,0:leqmax))

    call adios_get_i("ni", ni)
    call adios_get_i("nis", nis)
    call adios_get_i("ne", ne)
    call adios_get_r("delta", delta)
    call adios_get_r("rc", rc)
    call adios_get_r("fti", fti)
    call adios_get_r("fte", fte)

    call adios_get_r1("qq", qq)
    call adios_get_r1("qqinv", qqinv)
    call adios_get_r1("preq", preq)
    call adios_get_r1("feq", feq)
    call adios_get_r1("cureq", cureq)
    call adios_get_r1("denseq", denseq)
    call adios_get_r1("teeq", teeq)
    call adios_get_r1("nfeq", nfeq)
    call adios_get_r1("vfova", vfova)
    call adios_get_r1("vzt_eq", vzt_eq)
    call adios_get_r1("tieq", tieq)
    call adios_get_r1("vth_eq", vth_eq)
    if (alpha_on == 1) then
       call adios_get_r1("nalpeq", nalpeq)
       call adios_get_r1("valphaova", valphaova)
    end if
    if (ext_prof == 1) then
       allocate (vAlfven(0:mj),vtherm_elecP(0:mj))
       call adios_get_r1("vAlfven", vAlfven)
       call adios_get_r1("vtherm_elecP", vtherm_elecP)
    else
       call adios_get_r1("vtherm_elc", vtherm_elc)
    end if
    call adios_get_sarr("ndevice", ndevice)
    call adios_get_r("eps", eps)
    call adios_get_r("bet0", bet0)
    call adios_get_r("bet0_f", bet0_f)
    call adios_get_r1("rs", rs(1:lcnt))

    call adios_get_r2("sqgi", sqgi(:,1:leqmax))
    call adios_get_r2("sqg", sqg(:,1:leqmax))
    call adios_get_r2("bst", bst(:,1:leqmax))
    call adios_get_r2("grr", grr(:,1:leqmax))
    call adios_get_r2("grt", grt(:,1:leqmax))
    call adios_get_r2("gtt", gtt(:,1:leqmax))
    call adios_get_r2("grroj", grroj(:,1:leqmax))
    call adios_get_r2("grtoj", grtoj(:,1:leqmax))
    call adios_get_r2("gttoj", gttoj(:,1:leqmax))
    call adios_get_r2("jbgrr", jbgrr(:,1:leqmax))
    call adios_get_r2("jbgrt", jbgrt(:,1:leqmax))
    call adios_get_r2("jbgtt", jbgtt(:,1:leqmax))
    call adios_get_r2("lplrr", lplrr(:,1:leqmax))
    call adios_get_r2("lplrt", lplrt(:,1:leqmax))
    call adios_get_r2("lplrz", lplrz(:,1:leqmax))
    call adios_get_r2("lpltt", lpltt(:,1:leqmax))
    call adios_get_r2("lpltz", lpltz(:,1:leqmax))
    call adios_get_r2("lplzz", lplzz(:,1:leqmax))
    call adios_get_r2("lplr", lplr(:,1:leqmax))
    call adios_get_r2("lplt", lplt(:,1:leqmax))
    call adios_get_r2("lplz", lplz(:,1:leqmax))
    call adios_get_r2("djroj", djroj(:,1:leqmax))
    call adios_get_r2("djtoj", djtoj(:,1:leqmax))
    call adios_get_r2("djzoj", djzoj(:,1:leqmax))
    call adios_get_r2("omdr", omdr(:,1:leqmax))
    call adios_get_r2("omdt", omdt(:,1:leqmax))
    call adios_get_r2("omdz", omdz(:,1:leqmax))
    call adios_get_r2("bmod", bmod(:,1:leqmax))
    call adios_get_r2("dbsjtoj", dbsjtoj(:,1:leqmax))
    call adios_get_r2("dbsjzoj", dbsjzoj(:,1:leqmax))

    if (ieldamp_on == 1) then
       allocate (eildr(0:mj,0:leqmax),eildt(0:mj,0:leqmax),eildz(0:mj,0:leqmax),eildrr(0:mj,0:leqmax), &
            eildrt(0:mj,0:leqmax),eildrz(0:mj,0:leqmax),eildtt(0:mj,0:leqmax),eildtz(0:mj,0:leqmax), &
            eildzz(0:mj,0:leqmax))
       call adios_get_r2("eildr", eildr(:,1:leqmax))
       call adios_get_r2("eildt", eildt(:,1:leqmax))
       call adios_get_r2("eildz", eildz(:,1:leqmax))
       call adios_get_r2("eildrr", eildrr(:,1:leqmax))
       call adios_get_r2("eildrt", eildrt(:,1:leqmax))
       call adios_get_r2("eildrz", eildrz(:,1:leqmax))
       call adios_get_r2("eildtt", eildtt(:,1:leqmax))
       call adios_get_r2("eildtz", eildtz(:,1:leqmax))
       call adios_get_r2("eildzz", eildzz(:,1:leqmax))
    end if
    if (trapped_on == 1) then
       allocate (omdrprp(0:mj,0:leqmax),omdtprp(0:mj,0:leqmax),omdzprp(0:mj,0:leqmax))
       call adios_get_r2("omdrprp", omdrprp(:,1:leqmax))
       call adios_get_r2("omdtprp", omdtprp(:,1:leqmax))
       call adios_get_r2("omdzprp", omdzprp(:,1:leqmax))
    end if

    call adios_get_r1("eta", eta)
    call adios_get_r("dt", dt)
    call adios_get_r("time", time)

  end subroutine rddump

  subroutine rddump_flds

    ! Phase 2 of the restart read (called from resume, after setmod/mmlims
    ! and the field-array allocations that need mj_start/mj_end/lmax): reads
    ! the 8 dynamical fields (using an ADIOS2 selection to fetch only this
    ! rank's mj_start:mj_end radial slice directly, replacing the old
    ! read-everything-then-discard dummy reads) and the trailing scalar/flag
    ! records, then closes the engine opened by rddump.

    use processor
    use var_para

    implicit none

    integer :: j0, n0, lcnt

    if (myPE == 0) then
       j0 = 0
       n0 = mj+1
    else
       j0 = mj_start
       n0 = mj_end-mj_start+1
    end if
    lcnt = min(lmax,lmaxo)

    call adios_get_r2_slice("psi", psi(:,1:lcnt), j0, n0)
    call adios_get_r2_slice("phi", phi(:,1:lcnt), j0, n0)
    call adios_get_r2_slice("pr", pr(:,1:lcnt), j0, n0)
    call adios_get_r2_slice("nf", nf(:,1:lcnt), j0, n0)
    call adios_get_r2_slice("vprlf", vprlf(:,1:lcnt), j0, n0)
    call adios_get_r2_slice("vthprlf", vthprlf(:,1:lcnt), j0, n0)
    if (alpha_on == 1) then
       call adios_get_r2_slice("nalp", nalp(:,1:lcnt), j0, n0)
       call adios_get_r2_slice("vprlalp", vprlalp(:,1:lcnt), j0, n0)
    end if

    call adios_get_r("etascl", etascl)
    call adios_get_r("reta", reta)
    call adios_get_r("eta0", eta0)
    call adios_get_r("etalmb", etalmb)
    call adios_get_i("ietaeq", ietaeq)
    call adios_get_r("stdifp", stdifp)
    call adios_get_r("stdifu", stdifu)
    call adios_get_r("stdifnf", stdifnf)
    call adios_get_r("stdifv", stdifv)
    call adios_get_r("stdifvf", stdifvf)
    call adios_get_r("stdifnalp", stdifnalp)
    call adios_get_r("stdifvalp", stdifvalp)
    call adios_get_r("s", s)
    call adios_get_r("gamma", gamma)
    call adios_get_r("xle", xle)
    call adios_get_i("ipert", ipert)
    ! widthi/gammai/pertscl are dumped for provenance but (as in the
    ! original code) are always sourced from the namelist on resume, not
    ! restored here - matches historical behavior, not re-read.

    ! The original trailing record re-read ext_prof_name here, i.e. AFTER the
    ! namelists, so the dumped value overrides the namelist on resume.
    call adios_get_s("ext_prof_name", ext_prof_name)

    call adios_get_i("difnr_on", difnr_on)
    call adios_get_i("nopsievol_on", nopsievol_on)
    call adios_get_i("noprevol_on", noprevol_on)
    call adios_get_i("nonfevol_on", nonfevol_on)
    call adios_get_i("nonalpevol_on", nonalpevol_on)
    call adios_get_i("src_sink_th_on", src_sink_th_on)
    call adios_get_i("src_sink_EP1_on", src_sink_EP1_on)
    call adios_get_i("src_sink_EP2_on", src_sink_EP2_on)
    call adios_get_i("src_sink_DIIID_on", src_sink_DIIID_on)
    call adios_get_i("src_sink_ITER_on", src_sink_ITER_on)
    call adios_get_r("rsrc", rsrc)
    call adios_get_r("wsrc", wsrc)
    call adios_get_r("asrc", asrc)
    call adios_get_r("rsrc_EP1", rsrc_EP1)
    call adios_get_r("wsrc_EP1", wsrc_EP1)
    call adios_get_r("asrc_EP1", asrc_EP1)
    call adios_get_r("rsrc_EP2", rsrc_EP2)
    call adios_get_r("wsrc_EP2", wsrc_EP2)
    call adios_get_r("asrc_EP2", asrc_EP2)
    call adios_get_r("AWfctr", AWfctr)
    call adios_get_r("Nfctr", Nfctr)
    call adios_get_r("AWfctr_dif", AWfctr_dif)
    call adios_get_r("Rfctr", Rfctr)
    call adios_get_r("Wfctr", Wfctr)

    call adios_get_r1("srcsinkth", srcsinkth)
    call adios_get_r1("srcsinkEP1", srcsinkEP1)
    call adios_get_r1("srcsinkEP2", srcsinkEP2)

    call adios_close_engine

  end subroutine rddump_flds

  subroutine wrdump(i_diag, is_restart)

    ! Writes one self-describing ADIOS2 .bp restart dataset, replacing the
    ! old positional big_endian binary chain - one dataset for the
    ! final restart plus one appended-step series for periodic dumps, just
    ! a named-variable/attribute container instead of an unnamed byte
    ! stream. Named variables mean write order no longer has to match the
    ! reader at all.

    implicit none

    integer, intent(in) :: i_diag
    logical, intent(in) :: is_restart
    character(len=15) :: confil
    logical, save :: first_call = .true.
    logical, save :: series_started = .false.

    ! Periodic dumps are appended as successive steps of one dataset named
    ! after the first run of the series (numruns, carried through the restart
    ! files), so a continuation run appends to the original file. Only a new
    ! run (nstres == 0) truncates any stale file on its first dump.
    if (dumps_file == ' ') dumps_file = "fs"//numruns(1)//numruns(2)//"_dumps.bp"
    if (first_call) then
       series_started = (nstres /= 0)
       first_call = .false.
    end if
    ! The final restart dump stays a separate single-step file.
    if (is_restart) then
       confil="fs"//numrun(1)//numrun(2)//trim(numrun(3))//".bp"
       call adios_open_write(confil)
    else
       confil=dumps_file
       call adios_open_write(confil, series_started)
       series_started = .true.
    end if

    call adios_put_i("ihist", ihist)
    call adios_put_sarr("numrun", numrun)
    call adios_put_sarr("numruno", numruno)
    call adios_put_sarr("numruns", numruns)
    call adios_put_s("dumps_file", dumps_file)
    call adios_put_sarr("numhist", numhist(1:ihist))
    call adios_put_i("maxstp", maxstp)
    call adios_put_i("nstep", nstep)
    call adios_put_i("ndump", ndump)
    call adios_put_i("nprint", nprint)
    call adios_put_i("lplots", lplots)
    call adios_put_i("itime", itime)
    call adios_put_r("dt0", dt0)
    call adios_put_i("nstep1", nstep1)
    call adios_put_i("nonlin", nonlin)
    call adios_put_i("m0dy", m0dy)
    call adios_put_i("nocpl", nocpl)
    call adios_put_i("noprevol_on", noprevol_on)
    call adios_put_r("Adens", Adens)
    call adios_put_r("Bdens", Bdens)
    call adios_put_r("LcA0", LcA0)
    call adios_put_r("LcA1", LcA1)
    call adios_put_r("LcA2", LcA2)
    call adios_put_r("LcA3", LcA3)
    call adios_put_r("omcy", omcy)
    call adios_put_r("dpres", dpres)
    call adios_put_i("ext_prof", ext_prof)
    call adios_put_i("epflr_on", epflr_on)
    call adios_put_r("r_epflr", r_epflr)
    call adios_put_i("alpha_on", alpha_on)
    call adios_put_i("iflr_on", iflr_on)
    call adios_put_r("iflr", iflr)
    call adios_put_i("twofl_on", twofl_on)
    call adios_put_i("ieldamp_on", ieldamp_on)
    call adios_put_r("omegar", omegar)
    call adios_put_r("bet0_alp", bet0_alp)
    call adios_put_r("Adensalp", Adensalp)
    call adios_put_r("Bdensalp", Bdensalp)
    call adios_put_r("LcA0alp", LcA0alp)
    call adios_put_r("LcA1alp", LcA1alp)
    call adios_put_r("LcA2alp", LcA2alp)
    call adios_put_r("LcA3alp", LcA3alp)
    call adios_put_r("omcyalp", omcyalp)
    call adios_put_r("r_epflralp", r_epflralp)
    call adios_put_s("ext_prof_name", ext_prof_name)
    call adios_put_i("trapped_on", trapped_on)
    call adios_put_r("omcyb", omcyb)
    call adios_put_r("rbound", rbound)
    call adios_put_i("B_par_on", B_par_on)

    call adios_put_i("mj", mj)
    call adios_put_i("lmax", lmax)
    call adios_put_i("leqmax", leqmax)
    call adios_put_i("mjm1", mjm1)

    call adios_put_r1("r", r)
    call adios_put_i1("mm", mm(1:lmax))
    call adios_put_i1("nn", nn(1:lmax))
    call adios_put_i1("mmeq", mmeq)
    call adios_put_i1("nneq", nneq)
    call adios_put_r1("rinv", rinv)
    call adios_put_r1("dc1m", dc1m)
    call adios_put_r1("dc1p", dc1p)
    call adios_put_r1("dc2m", dc2m)
    call adios_put_r1("dc2p", dc2p)
    call adios_put_r1("del2cm", del2cm)
    call adios_put_r1("del2cp", del2cp)

    call adios_put_i("ni", ni)
    call adios_put_i("nis", nis)
    call adios_put_i("ne", ne)
    call adios_put_r("delta", delta)
    call adios_put_r("rc", rc)
    call adios_put_r("fti", fti)
    call adios_put_r("fte", fte)

    call adios_put_r1("qq", qq)
    call adios_put_r1("qqinv", qqinv)
    call adios_put_r1("preq", preq)
    call adios_put_r1("feq", feq)
    call adios_put_r1("cureq", cureq)
    call adios_put_r1("denseq", denseq)
    call adios_put_r1("teeq", teeq)
    call adios_put_r1("nfeq", nfeq)
    call adios_put_r1("vfova", vfova)
    call adios_put_r1("vzt_eq", vzt_eq)
    call adios_put_r1("tieq", tieq)
    call adios_put_r1("vth_eq", vth_eq)
    if (alpha_on == 1) then
       call adios_put_r1("nalpeq", nalpeq)
       call adios_put_r1("valphaova", valphaova)
    end if
    if (ext_prof == 1) then
       call adios_put_r1("vAlfven", vAlfven)
       call adios_put_r1("vtherm_elecP", vtherm_elecP)
    else
       call adios_put_r1("vtherm_elc", vtherm_elc)
    end if
    call adios_put_sarr("ndevice", ndevice)
    call adios_put_r("eps", eps)
    call adios_put_r("bet0", bet0)
    call adios_put_r("bet0_f", bet0_f)
    call adios_put_r1("rs", rs(1:lmax))

    call adios_put_r2("sqgi", sqgi(:,1:leqmax))
    call adios_put_r2("sqg", sqg(:,1:leqmax))
    call adios_put_r2("bst", bst(:,1:leqmax))
    call adios_put_r2("grr", grr(:,1:leqmax))
    call adios_put_r2("grt", grt(:,1:leqmax))
    call adios_put_r2("gtt", gtt(:,1:leqmax))
    call adios_put_r2("grroj", grroj(:,1:leqmax))
    call adios_put_r2("grtoj", grtoj(:,1:leqmax))
    call adios_put_r2("gttoj", gttoj(:,1:leqmax))
    call adios_put_r2("jbgrr", jbgrr(:,1:leqmax))
    call adios_put_r2("jbgrt", jbgrt(:,1:leqmax))
    call adios_put_r2("jbgtt", jbgtt(:,1:leqmax))
    call adios_put_r2("lplrr", lplrr(:,1:leqmax))
    call adios_put_r2("lplrt", lplrt(:,1:leqmax))
    call adios_put_r2("lplrz", lplrz(:,1:leqmax))
    call adios_put_r2("lpltt", lpltt(:,1:leqmax))
    call adios_put_r2("lpltz", lpltz(:,1:leqmax))
    call adios_put_r2("lplzz", lplzz(:,1:leqmax))
    call adios_put_r2("lplr", lplr(:,1:leqmax))
    call adios_put_r2("lplt", lplt(:,1:leqmax))
    call adios_put_r2("lplz", lplz(:,1:leqmax))
    call adios_put_r2("djroj", djroj(:,1:leqmax))
    call adios_put_r2("djtoj", djtoj(:,1:leqmax))
    call adios_put_r2("djzoj", djzoj(:,1:leqmax))
    call adios_put_r2("omdr", omdr(:,1:leqmax))
    call adios_put_r2("omdt", omdt(:,1:leqmax))
    call adios_put_r2("omdz", omdz(:,1:leqmax))
    call adios_put_r2("bmod", bmod(:,1:leqmax))
    call adios_put_r2("dbsjtoj", dbsjtoj(:,1:leqmax))
    call adios_put_r2("dbsjzoj", dbsjzoj(:,1:leqmax))

    if (ieldamp_on == 1) then
       call adios_put_r2("eildr", eildr(:,1:leqmax))
       call adios_put_r2("eildt", eildt(:,1:leqmax))
       call adios_put_r2("eildz", eildz(:,1:leqmax))
       call adios_put_r2("eildrr", eildrr(:,1:leqmax))
       call adios_put_r2("eildrt", eildrt(:,1:leqmax))
       call adios_put_r2("eildrz", eildrz(:,1:leqmax))
       call adios_put_r2("eildtt", eildtt(:,1:leqmax))
       call adios_put_r2("eildtz", eildtz(:,1:leqmax))
       call adios_put_r2("eildzz", eildzz(:,1:leqmax))
    end if
    if (trapped_on == 1) then
       call adios_put_r2("omdrprp", omdrprp(:,1:leqmax))
       call adios_put_r2("omdtprp", omdtprp(:,1:leqmax))
       call adios_put_r2("omdzprp", omdzprp(:,1:leqmax))
    end if

    call adios_put_r1("eta", eta)
    call adios_put_r("dt", dt)
    call adios_put_r("time", time)

    call adios_put_r2("psi", psi(:,1:lmax))
    call adios_put_r2("phi", phi(:,1:lmax))
    call adios_put_r2("pr", pr(:,1:lmax))
    call adios_put_r2("nf", nf(:,1:lmax))
    call adios_put_r2("vprlf", vprlf(:,1:lmax))
    call adios_put_r2("vthprlf", vthprlf(:,1:lmax))
    if (alpha_on == 1) then
       call adios_put_r2("nalp", nalp(:,1:lmax))
       call adios_put_r2("vprlalp", vprlalp(:,1:lmax))
    end if

    call adios_put_r("etascl", etascl)
    call adios_put_r("reta", reta)
    call adios_put_r("eta0", eta0)
    call adios_put_r("etalmb", etalmb)
    call adios_put_i("ietaeq", ietaeq)
    call adios_put_r("stdifp", stdifp)
    call adios_put_r("stdifu", stdifu)
    call adios_put_r("stdifnf", stdifnf)
    call adios_put_r("stdifv", stdifv)
    call adios_put_r("stdifvf", stdifvf)
    call adios_put_r("stdifnalp", stdifnalp)
    call adios_put_r("stdifvalp", stdifvalp)
    call adios_put_r("s", s)
    call adios_put_r("gamma", gamma)
    call adios_put_r("xle", xle)
    call adios_put_i("ipert", ipert)
    call adios_put_r1("widthi", widthi)
    call adios_put_r1("gammai", gammai)
    call adios_put_r("pertscl", pertscl)

    call adios_put_i("difnr_on", difnr_on)
    call adios_put_i("nopsievol_on", nopsievol_on)
    call adios_put_i("nonfevol_on", nonfevol_on)
    call adios_put_i("nonalpevol_on", nonalpevol_on)
    call adios_put_i("src_sink_th_on", src_sink_th_on)
    call adios_put_i("src_sink_EP1_on", src_sink_EP1_on)
    call adios_put_i("src_sink_EP2_on", src_sink_EP2_on)
    call adios_put_i("src_sink_DIIID_on", src_sink_DIIID_on)
    call adios_put_i("src_sink_ITER_on", src_sink_ITER_on)
    call adios_put_r("rsrc", rsrc)
    call adios_put_r("wsrc", wsrc)
    call adios_put_r("asrc", asrc)
    call adios_put_r("rsrc_EP1", rsrc_EP1)
    call adios_put_r("wsrc_EP1", wsrc_EP1)
    call adios_put_r("asrc_EP1", asrc_EP1)
    call adios_put_r("rsrc_EP2", rsrc_EP2)
    call adios_put_r("wsrc_EP2", wsrc_EP2)
    call adios_put_r("asrc_EP2", asrc_EP2)
    call adios_put_r("AWfctr", AWfctr)
    call adios_put_r("Nfctr", Nfctr)
    call adios_put_r("AWfctr_dif", AWfctr_dif)
    call adios_put_r("Rfctr", Rfctr)
    call adios_put_r("Wfctr", Wfctr)

    call adios_put_r1("srcsinkth", srcsinkth)
    call adios_put_r1("srcsinkEP1", srcsinkEP1)
    call adios_put_r1("srcsinkEP2", srcsinkEP2)

    call adios_close_engine
    write (6,'(/"  wrdump: have written ",a)') trim(confil)

  end subroutine wrdump


!======================================================================
! LEGACY TEXT OUTPUT (pre-ADIOS2). Kept so it can be switched back on:
!   uncomment (delete the "!LEG " prefix) and call endrun_text instead of endrun
!======================================================================
!LEG   subroutine endrun_text
!LEG
!LEG     use processor
!LEG     use var_para
!LEG     use dbyd
!LEG     use mult_mod
!LEG     use transfer
!LEG
!LEG     implicit none
!LEG
!LEG     integer :: i,j,l,lwrt,l2t,mnum
!LEG     real(IDP) :: scmn,scmx,sclmn,sclmx
!LEG     real(IDP), dimension(:), allocatable :: scnorm
!LEG     character(len=1) :: tb
!LEG     character(len=12) :: confil
!LEG     character(len=32) :: formatt='("r",1200(a1,i4,"/",i4))'
!LEG     character(len=32) :: formatv='(1pe13.6,1200(a1,1pe15.8))'
!LEG
!LEG     interface
!LEG        subroutine delstar(ss,ff,itypf,wk1,wk2,wkeq1,c1,c2)
!LEG          use param
!LEG          use var_para
!LEG          implicit none
!LEG          integer :: itypf
!LEG          real(IDP) :: c1,c2
!LEG          real(IDP), dimension(mj_start:,0:) :: ss,ff,wk1,wk2
!LEG          real(IDP), dimension(0:,0:) :: wkeq1
!LEG        end subroutine delstar
!LEG     end interface
!LEG
!LEG     tb=char(9)
!LEG
!LEG     !  store uzt-values into sc4
!LEG
!LEG     do l=1,lmax
!LEG        sc4(:,l)=uzt(mj_start:mj_end,l)
!LEG     end do
!LEG
!LEG     allocate (scnorm(nnum))
!LEG     scnorm=1.0_IDP
!LEG
!LEG     if (myPE == 0) then
!LEG
!LEG        !  vr up
!LEG        call dbydth(uzt,phi,-1,0.0_IDP,-eps,0)
!LEG
!LEG        do i=1,nnum
!LEG           mnum=mnumn(i)
!LEG           if (nonlin == 0) then
!LEG              scmn=0.
!LEG              scmx=0.
!LEG              do l2t=1,mnum
!LEG                 l=l2t+lnumn(i-1)
!LEG                 sclmn=minval(uzt(:,lln(l)))
!LEG                 sclmx=maxval(uzt(:,lln(l)))
!LEG                 scmn=min(scmn,sclmn)
!LEG                 scmx=max(scmx,sclmx)
!LEG              end do
!LEG              if (scmx > abs(scmn)) then
!LEG                 scnorm(i)=scmx
!LEG              else
!LEG                 scnorm(i)=scmn
!LEG              endif
!LEG           end if
!LEG           if(scnorm(i) /= 0.0_IDP) then
!LEG              do l2t=1,mnum
!LEG                 l=l2t+lnumn(i-1)
!LEG                 psi(:,lln(l))=psi(:,lln(l))/scnorm(i)
!LEG                 where (abs(psi(:,lln(l))) < 1.e-50_IDP) psi(:,lln(l))=0
!LEG                 phi(:,lln(l))=phi(:,lln(l))/scnorm(i)
!LEG                 where (abs(phi(:,lln(l))) < 1.e-50_IDP) phi(:,lln(l))=0
!LEG                 pr(:,lln(l))=pr(:,lln(l))/scnorm(i)
!LEG                 where (abs(pr(:,lln(l))) < 1.e-50_IDP) pr(:,lln(l))=0
!LEG                 nf(:,lln(l))=nf(:,lln(l))/scnorm(i)
!LEG                 where (abs(nf(:,lln(l))) < 1.e-50_IDP) nf(:,lln(l))=0
!LEG                 vprlf(:,lln(l))=vprlf(:,lln(l))/scnorm(i)
!LEG                 where (abs(vprlf(:,lln(l))) < 1.e-50_IDP) vprlf(:,lln(l))=0
!LEG                 vthprlf(:,lln(l))=vthprlf(:,lln(l))/scnorm(i)
!LEG                 where (abs(vthprlf(:,lln(l))) < 1.e-50_IDP) vthprlf(:,lln(l))=0
!LEG                 uzt(:,lln(l))=uzt(:,lln(l))/scnorm(i)
!LEG                 where (abs(uzt(:,lln(l))) < 1.e-50_IDP) uzt(:,lln(l))=0
!LEG              end do
!LEG              if (alpha_on == 1) then
!LEG                 do l2t=1,mnum
!LEG                    l=l2t+lnumn(i-1)
!LEG                    nalp(:,lln(l))=nalp(:,lln(l))/scnorm(i)
!LEG                    where (abs(nalp(:,lln(l))) < 1.e-50_IDP) nalp(:,lln(l))=0
!LEG                    vprlalp(:,lln(l))=vprlalp(:,lln(l))/scnorm(i)
!LEG                    where (abs(vprlalp(:,lln(l))) < 1.e-50_IDP) vprlalp(:,lln(l))=0
!LEG                 end do
!LEG              end if
!LEG           end if
!LEG        end do
!LEG        lwrt=abs(lplots)
!LEG        if (lwrt > lmax) lwrt=lmax
!LEG        write(confil,'("phi_",2a2)') numrun(1),numrun(2)
!LEG        open(unit=92,file=confil,recl=19384)
!LEG        write(92,formatt) (tb,mm(l),nn(l),l=1,lwrt)
!LEG        do j=0,mj
!LEG           write(92,formatv) r(j),(tb,phi(j,l),l=1,lwrt)
!LEG        end do
!LEG        close(92)
!LEG        write(confil,'("psi_",2a2)') numrun(1),numrun(2)
!LEG        open(unit=92,file=confil,recl=19384)
!LEG        write(92,formatt) (tb,mm(l),nn(l),l=1,lwrt)
!LEG        do j=0,mj
!LEG           write(92,formatv) r(j),(tb,psi(j,l),l=1,lwrt)
!LEG        end do
!LEG        close(92)
!LEG        write(confil,'("pr_",2a2)') numrun(1),numrun(2)
!LEG        open(unit=92,file=confil,recl=19384)
!LEG        write(92,formatt) (tb,mm(l),nn(l),l=1,lwrt)
!LEG        do j=0,mj
!LEG           write(92,formatv) r(j),(tb,pr(j,l),l=1,lwrt)
!LEG        end do
!LEG        close(92)
!LEG        write(confil,'("vr_",2a2)') numrun(1),numrun(2)
!LEG        open(unit=92,file=confil,recl=19384)
!LEG        write(92,formatt) (tb,mm(l),nn(l),l=1,lwrt)
!LEG        do j=0,mj
!LEG           write(92,formatv) r(j),(tb,uzt(j,l),l=1,lwrt)
!LEG        end do
!LEG        close(92)
!LEG        write(confil,'("nf_",2a2)') numrun(1),numrun(2)
!LEG        open(unit=92,file=confil,recl=19384)
!LEG        write(92,formatt) (tb,mm(l),nn(l),l=1,lwrt)
!LEG        do j=0,mj
!LEG           write(92,formatv) r(j),(tb,nf(j,l),l=1,lwrt)
!LEG        end do
!LEG        close(92)
!LEG        write(confil,'("vprlf_",2a2)') numrun(1),numrun(2)
!LEG        open(unit=92,file=confil,recl=19384)
!LEG        write(92,formatt) (tb,mm(l),nn(l),l=1,lwrt)
!LEG        do j=0,mj
!LEG           write(92,formatv) r(j),(tb,vprlf(j,l),l=1,lwrt)
!LEG        end do
!LEG        close(92)
!LEG        write(confil,'("vthprlf_",2a2)') numrun(1),numrun(2)
!LEG        open(unit=92,file=confil,recl=19384)
!LEG        write(92,formatt) (tb,mm(l),nn(l),l=1,lwrt)
!LEG        do j=0,mj
!LEG           write(92,formatv) r(j),(tb,vthprlf(j,l),l=1,lwrt)
!LEG        end do
!LEG        close(92)
!LEG        if (alpha_on == 1) then
!LEG           write(confil,'("nalp_",2a2)') numrun(1),numrun(2)
!LEG           open(unit=92,file=confil,recl=19384)
!LEG           write(92,formatt) (tb,mm(l),nn(l),l=1,lwrt)
!LEG           do j=0,mj
!LEG              write(92,formatv) r(j),(tb,nalp(j,l),l=1,lwrt)
!LEG           end do
!LEG           close(92)
!LEG           write(confil,'("vprlalp_",2a2)') numrun(1),numrun(2)
!LEG           open(unit=92,file=confil,recl=19384)
!LEG           write(92,formatt) (tb,mm(l),nn(l),l=1,lwrt)
!LEG           do j=0,mj
!LEG              write(92,formatv) r(j),(tb,vprlalp(j,l),l=1,lwrt)
!LEG           end do
!LEG           close(92)
!LEG        end if
!LEG
!LEG        !  vth up
!LEG        call dbydr(uzt,phi,0.0_IDP,eps,0)
!LEG        write(confil,'("vth_",2a2)') numrun(1),numrun(2)
!LEG        open(unit=92,file=confil,recl=19384)
!LEG        write(92,formatt) (tb,mm(l),nn(l),l=1,lwrt)
!LEG        do j=0,mj
!LEG           write(92,formatv) r(j),(tb,uzt(j,l),l=1,lwrt)
!LEG        end do
!LEG        close(92)
!LEG     end if
!LEG
!LEG     !  br up
!LEG     call dbydth_par(sc3,psi,1,0.0_IDP,-eps,0)
!LEG     !  bth up
!LEG     call dbydr_par(sc5,psi,0.0_IDP,eps,0)
!LEG
!LEG     call multed(uzt,sqgi,1,sc3,-1,0.0_IDP,1.0_IDP)
!LEG     call trnsfr0(uzt,-1)
!LEG     if (myPE == 0) then
!LEG        write(confil,'("br_",2a2)') numrun(1),numrun(2)
!LEG        open(unit=92,file=confil,recl=19384)
!LEG        write(92,formatt) (tb,mm(l),nn(l),l=1,lwrt)
!LEG        do j=0,mj
!LEG           write(92,formatv) r(j),(tb,uzt(j,l),l=1,lwrt)
!LEG        end do
!LEG        close(92)
!LEG     end if
!LEG
!LEG     call multed(uzt,sqgi,1,sc5,1,0.0_IDP,1.0_IDP)
!LEG     call trnsfr0(uzt,1)
!LEG     if (myPE == 0) then
!LEG        write(confil,'("bth_",2a2)') numrun(1),numrun(2)
!LEG        open(unit=92,file=confil,recl=19384)
!LEG        write(92,formatt) (tb,mm(l),nn(l),l=1,lwrt)
!LEG        do j=0,mj
!LEG           write(92,formatv) r(j),(tb,uzt(j,l),l=1,lwrt)
!LEG        end do
!LEG        close(92)
!LEG     end if
!LEG
!LEG     call delstar(uzt,psi,1,sc2,sc3,sceq1,0.0_IDP,1.0_IDP)
!LEG     uzt=eps*uzt
!LEG     call trnsfr0(uzt,1)
!LEG     if (myPE == 0) then
!LEG        write(confil,'("curzt_",2a2)') numrun(1),numrun(2)
!LEG        open(unit=92,file=confil,recl=19384)
!LEG        write(92,formatt) (tb,mm(l),nn(l),l=1,lwrt)
!LEG        do j=0,mj
!LEG           write(92,formatv) r(j),(tb,uzt(j,l),l=1,lwrt)
!LEG        end do
!LEG        close(92)
!LEG     end if
!LEG
!LEG     !  restore uzt-values
!LEG
!LEG     do l=1,lmax
!LEG        uzt(mj_start:mj_end,l)=sc4(:,l)
!LEG     end do
!LEG     call trnsfr0(uzt,1)
!LEG     if (myPE == 0) then
!LEG        do i=1,nnum
!LEG           mnum=mnumn(i)
!LEG           if (nonlin == 0) then
!LEG              scmn=0.
!LEG              scmx=0.
!LEG              do l2t=1,mnum
!LEG                 l=l2t+lnumn(i-1)
!LEG                 sclmn=minval(uzt(:,lln(l)))
!LEG                 sclmx=maxval(uzt(:,lln(l)))
!LEG                 scmn=min(scmn,sclmn)
!LEG                 scmx=max(scmx,sclmx)
!LEG              end do
!LEG              if (scmx > abs(scmn)) then
!LEG                 scnorm(i)=scmx
!LEG              else
!LEG                 scnorm(i)=scmn
!LEG              endif
!LEG           end if
!LEG           if(scnorm(i) /= 0.0_IDP) then
!LEG              do l2t=1,mnum
!LEG                 l=l2t+lnumn(i-1)
!LEG                 uzt(:,lln(l))=uzt(:,lln(l))/scnorm(i)
!LEG                 where (abs(uzt(:,lln(l))) < 1.e-50_IDP) uzt(:,lln(l))=0
!LEG              end do
!LEG           end if
!LEG        end do
!LEG        write(confil,'("uzt_",2a2)') numrun(1),numrun(2)
!LEG        open(unit=92,file=confil,recl=19384)
!LEG        write(92,formatt) (tb,mm(l),nn(l),l=1,lwrt)
!LEG        do j=0,mj
!LEG           write(92,formatv) r(j),(tb,uzt(j,l),l=1,lwrt)
!LEG        end do
!LEG        close(92)
!LEG     end if
!LEG
!LEG   end subroutine endrun_text

END MODULE output_mod
