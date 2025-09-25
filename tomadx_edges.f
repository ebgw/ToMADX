c read a mad8 output and write a madx input as sequence
c the mad8 output is the file obtained by the command
c         STRUCTURE,FILENAME=lattice.dat,ORDER=5
c
c s is in madx and mad8 the arc length, but
c in this file the magnet position is instead the sum of the
c lengths! This is why here I compute posm myself as the arc length.
c In a sequence the position refers to the arc length
c nb: when defining a bend as rbend, both mad8 (at least some versions) and madx 
c     understand that l is the cord, not the arc length
c     when it is a sbend madx and mad8 understand that l is the arc length
c MULTIPOLES with all terms smaller then user chosen delta are skipped


c After changing the MAC operating system, the "-" pitfall is present also by reading numbers
c Before gave error only when reading numbers from a text string as in
c       MULTQMP605B1            0.000000 0.000000000E+00-0.000000000E+00
c now also reading 
c  0.000000000E+00 0.000000000E+00 0.000000000E+00-1.017900000E-02 0.000000000E+00
c as numbers gives: Fortran runtime error: Bad real number in item 3 of list input
c therefore read it as string and use subroutine check
c     One more pitfall: it is recommended to not use LATTICE.DAT created on other platforms!

c     In the input file produced by mad8 the rbend length is the cord.
c     For computing correctly the position of the rbend for madx sequence
c     the arc must be computed

c Added symbolic values for strenghts. The actual values are written in values.dat
c Added writing of dummy/multi_sext.dat     

      implicit real*8 (a-h,o-z)
      parameter(maxel=10000)
      character*60 namel(maxel)
      character*130 buf, text
      character*1 ans,ansd,ansm,anss,ansb,ansk
      character*18 command
      data command/"rm dummy/tempo.seq"/

      character*60 name,type
      common/param/pos,posm,rl,arc,angle,rk1,rk2,rk3,tilt,tv,d3,name,
     *             type
      common/pmultn/rknl0,rknl1,rknl2,rknl3,rknl4,rknl5
      common/pmults/rksl0,rksl1,rksl2,rksl3,rksl4,rksl5

      data delta/1.d-14/

      character*60 nend
      common/endel/nend,nchar
      data nend/'DRIFDFINAL2'/,nchar/11/

c read file produced by mad8 command 
c         STRUCTURE,FILENAME=lattice.dat,ORDER=5
c in this file pos is not the arc but the sum of the lengths
      
      character*20 seqnam
      data totl/0.0/,seqnam/'MI_TEST'/

      write(6,500)
 500  format('Reading:',/,'  LATTICE.DAT',/,
     *       'created by mad8 command STUCTURE with order=5',/,
     *       'Writing: values.dat, madx.def and madx.seq',//,
     *       'Describe rbends as sbends+edges? (y/n)')
      read(5,'(a1)')ans
      write(6,*)'Include drifts?  (y/n)'
      read(5,'(a1)')ansd
      write(6,*)'Enter smallest absolute value of multipole to be kept'
      read(5,*)delta
c      ansm='y'
c      if(delta.eq.0)ansm='n'
c      write(6,*)'Switch skew multipoles signs? (y/n) '
c answer "no", the switch was done by mad8 when dumping the data in LATTICE.DAT
c     read(5,'(a1)')anss
      anss='n'
      write(6,*)'Switch off skew quad multipoles? (y/n) '
      read(5,'(a1)')ansk      
      


c input
      open(unit=10,file="LATTICE.DAT")
c output
c later dummy/tempo.seq is copied into dummy/madx.seq for adding the sequence length
      open(unit=11,file="dummy/madx.def")
      open(unit=12,file="dummy/tempo.seq")
      open(unit=16,file="tomadx.log")
      open(unit=18,file="test_pos.out")
      open(unit=22,file="dummy/multipoles_madx.def")
      open(unit=23,file="dummy/markers.def")
      open(unit=14,file="dummy/values.dat")
      open(unit=24,file="dummy/multi_sext.dat")
      open(unit=25,file="dummy/sext.dat")      
      
c      write(12,200) seqnam,totl
 200  format(a20,' : sequence, REFER=CENTRE, L=',d17.12,';')

      read(10,*,end=90)
      read(10,*,end=90)
      nel=0
      nty=0
      posm=0
      kmex=0
      kskex=0
      nmult=0
 1    continue
      read(10,'(a)',end=2)text
      i0=1
      call findnam(text,name,type,i0)
      nel=nel+1
      buf=''
      buf=text(i0:96)
      call check(buf)
      if(type.eq.'MULTIPOLE')then
         nmult=nmult+1
         read(unit=buf,fmt=*) rl,d1,d2,iord
         if(iord.ne.5)then
          write(6,*)'Maximum mult order must be 5!'
          stop
         end if
         read(10,'(a)')text
         call check(text)
         read(unit=text,fmt=*) rknl0,rknl1,rknl2,rknl3,rknl4
         read(10,*) rknl5
         read(10,'(a)')text
         call check(text)
         read(unit=text,fmt=*)rksl0,rksl1,rksl2,rksl3,rksl4
         read(10,*) rksl5
         read(10,'(a)')text
         call check(text)
         read(unit=text,fmt=*)d3,d4,d5,pos
         read(10,'(a)')text
         call check(text)
         read(unit=text,fmt=*)d7,d8,d9
c check whether there are multipoles with normal and skew components
         if((dabs(rknl0).gt.delta.or.
     *       dabs(rknl1).gt.delta.or.
     *       dabs(rknl2).gt.delta.or.
     *       dabs(rknl3).gt.delta.or.
     *       dabs(rknl4).gt.delta.or.
     *       dabs(rknl5).gt.delta).and.
     *      (dabs(rksl0).gt.delta.or.
     *       dabs(rksl1).gt.delta.or.
     *       dabs(rksl2).gt.delta.or.
     *       dabs(rksl3).gt.delta.or.
     *       dabs(rksl4).gt.delta.or.
     *       dabs(rksl5).gt.delta) )then
             write(16,123)name(1:20)
 123   format('Multipole ',a20,' is normal and skew!')
         end if
      else
         read(unit=buf,fmt=*)rl,angle,rk1,rk2
         read(10,'(a)')text
         call check(text)
         read(unit=text,fmt=*)tilt,tv,d3,d4,d5
         read(10,'(a)')text
         call check(text)
         read(unit=text,fmt=*) d6,d7,d8,pos
         read(10,'(a)')text
         call check(text)
         read(unit=text,fmt=*) d9,d10,d11
      end if
c pos is the arc length? not in this file!
c set default, it is changed for bending magnets in ewrite
c for bends compute it here if ifound=1 and therefore ewrite isn't called
      if(rl.lt.0)then
        write(6,*)"Negative length at: ",posm,rl
        write(6,'(a)')name
      end if
      arc=rl
      if(type.eq.'DRIFT'.and.ansd.eq.'n')then
          posm=posm+arc 
          go to 1
       end if
      call find(name,namel,nty,ifound)
      if(ifound.eq.1.and.type.eq.'RBEND')then
         write(6,*)'Computing arc length'
c compute the arc length for computing the position
         if(angle.ne.0)then
          rho=0.5*rl/dsin(angle*0.5)
          arc=rho*angle
         else
          arc=rl
         end if
      end if
      if(ifound.eq.1.and.type.eq.'SBEND')then
         write(6,*)'Computing arc length'
c compute the arc length for computing the position
         if(angle.ne.0)then
          rho=0.5*rl/dsin(angle*0.5)
          arc=rho*angle
         else
          arc=rl
         end if
      end if
      if(ifound.eq.0)then
c hkick and vkick are arguments of type kicker but they are NOT
c stored in LATTICE.DAT therefore I set them to zero. The values must be added by hand
c in the definition file
c     call ewrite(delta,ans,ansm,anss,ansk)
             call ewrite(delta,ans,anss,ansk)   
      end if
      posm=posm+arc 
c      if(type.eq.'MULTIPOLE')write(6,888)name,rknl0,rksl0
 888  format("found multipole:",a,2(1x,f10.8))
      if(type.eq.'MULTIPOLE'.and.rl.eq.0.)then
c       if(ansm.eq.'y')then
         dknl0=dabs(rknl0)
         dknl1=dabs(rknl1)
         dknl2=dabs(rknl2)
         dknl3=dabs(rknl3)
         dknl4=dabs(rknl4)
         dknl5=dabs(rknl5)
         dksl0=dabs(rksl0)
         dksl1=dabs(rksl1)
         dksl2=dabs(rksl2)
         dksl3=dabs(rksl3)
         dksl4=dabs(rksl4)
         dksl5=dabs(rksl5)
         if(dknl0.lt.delta.and.dknl1.lt.delta.and.dknl2.lt.delta.and.
     *      dknl3.lt.delta.and.dknl4.lt.delta.and.dknl5.lt.delta.and.
     *      dksl0.lt.delta.and.dksl1.lt.delta.and.dksl2.lt.delta.and.
     *      dksl3.lt.delta.and.dksl4.lt.delta.and.dksl5.lt.delta)then
            kmex=kmex+1
            go to 1 
         end if
c       end if
       if(ansk.eq.'y')then
        kskex=kskex+1
       end if
      end if
c write position on unit 12 for checks
        write(18,201)name,pos,posm,rl,arc
 201    format(a12,2x,4(f14.8,1x))
      call swrite(posm,rl,arc,name,type)
      if(name.eq.'MEND')go to 2
      go to 1

 2    continue
      write(6,*)'Sequence length (micron): ',posm*1.d6
      write(12,100)
 100  format('ENDSEQUENCE;')

      write(6,*)'Number of multipoles in lattice.dat: ',nmult
      write(6,*)'Number of skipped multipoles: ',kmex
      write(6,*)'Number of zeroed skew quad mutipoles: ',kskex
      write(6,900)
 900  format('WARNING: Edit first line of madx.seq ',
     *       'for assigning the sequence name !!!')

      close(10)
      close(11)
      close(12)
      close(16)
      close(18)
      close(22)
      close(23)
      close(14)
      close(24)      
      close(25)

c introduce first line which contains name and length of the sequence
      open(unit=10,file="dummy/tempo.seq")
      open(unit=11,file="dummy/madx.seq")
      write(11,200)seqnam,posm
 3    continue
      read(10,'(a)',end=4)text
      write(11,'(a)')text
      go to 3
 4    continue
      close(10)
      close(11)
c delete temporary file
      call system(command,istatus)
      if(istatus.eq.0)write(6,*)'Temporary file dummy/tempo.seq deleted'
      if(istatus.ne.0)write(6,*)'Unable to delete dummy/tempo.seq'      
      stop
      
 90   continue
      write(6,*)'Abnormal EoF on unit 10'
      stop
      end 

      subroutine findnam(text,name,type,i0)
      parameter(ntyp=17)
      implicit real*8 (a-h,o-z)
      character*60 name,type
      character*60 mytyp(ntyp),mytyps(ntyp)
      character*130 buf, text
      character*60 nend
      common/endel/nend,nchar
      data mytyp/'RBEND','SBEND','DRIFT','QUADRUPOLE','OCTUPOLE',
     *     'MARKER','HKICKER','VKICKER','RCOLLIMATOR','MONITOR',
     *     'HMONITOR','VMONITOR','INSTRUMENT','SEXTUPOLE','MULTIPOLE',
     *     'RFCAVITY','KICKER'/
      data mytyps/'RBEN','SBEN','DRIF','QUAD','OCTU',
     *     'MARK','HKIC','VKIC','RCOL','MONI',
     *     'HMON','VMON','INST','SEXT','MULT',
     *     'RFCA','KICK'/      
      
      do 1 i=1,124
      if(text(i:i+6).eq.'INITIAL')then
        name="MSTART"
        type="MARKER"
        i0=i+7
        return
      end if
 1    continue
      do 2 i=1,120
      if(text(i:i+nchar-1).eq.nend)then
        name="MEND"
        type="DRIFT"
        i0=i+nchar
        return
      end if
 2    continue
      do 3 i=1,130
      if(text(i:i).eq.' ')then
        type=''
        type=text(1:4)
        name=''
        name=text(5:i-1) 
        i0=i
c     complete type name
        do 4 j=1,ntyp
           if(type.eq.mytyps(j))then
              type=mytyp(j)
              return
           end if
 4      continue
        write(6,900)type
 900    format(a,' is unkwon element')
        return
       end if
 3    continue
      write(6,*)'Unrecognized name/type in string:'
      write(6,'(a)')text
      stop
      end
      subroutine find(name,namel,nty,ifound)
      parameter(maxel=10000)
      character*60 namel(maxel),name
      do 1 i=1,nty
      if(name.eq.namel(i))then
        ifound=1
        return
      end if
 1    continue
c element is new
      nty=nty+1
      namel(nty)=name
      ifound=0
      return
      end

c     subroutine ewrite(delta,ans,ansm,anss,ansk)
      subroutine ewrite(delta,ans,anss,ansk)      
      implicit real*8 (a-h,o-z)
      character*130 buf, text
      character*60 name,type
      character*1 ans,ansm,anss,ansk
      common/param/pos,posm,rl,arc,angle,rk1,rk2,rk3,tilt,tv,d3,name,
     *             type
      common/pmultn/rknl0,rknl1,rknl2,rknl3,rknl4,rknl5
      common/pmults/rksl0,rksl1,rksl2,rksl3,rksl4,rksl5

      pi=dacos(-1.d0)


      if(type.eq.'DRIFT')then
        write(11,100) name,type,rl
 100    format(a12,1x,' : ',a6,' , L=',f10.6,' ;')
c 120    format('D',5x,'1',f10.6,' 0.    0.   0.')
        return
      end if
      if(type.eq.'MARKER')then
        write(11,121) name,type
        write(23,121) name,type
 121    format(a12,1x,' : ',a6,' ;')
        return
      end if
      if(type.eq.'SBEND')then
c for SBEND madx and mad8 length is the arc
        arc=rl
        rho=arc/angle
        call countc(name,nch)
        write(14,300)name(1:nch),angle,name(1:nch),rk1,name(1:nch),rk2,
     *       name(1:nch),tilt
        write(11,102) name(1:nch),rl,name(1:nch),name(1:nch),
     *       name(1:nch),name(1:nch),tv,d3
 102    format(a,1x,' : SBEND, L=',e16.9,', ANGLE:=angle_',a,
     *        ', K1:=K1_',a,', K2:=K2_',a,', TILT:=tilt_',a,
     *   ', E1=',e16.9,', E2=',e16.9,' ;')             
        return
      end if

      if(type.eq.'RBEND')then
        call countc(name,nch)
        if(ans.ne.'y')then
c madx and mad8 length rl is the cord
         if(angle.ne.0)then
          rho=0.5*rl/dsin(angle*0.5)
          arc=rho*angle
          else
          arc=rl
       end if

         write(14,300)name(1:nch),angle,name(1:nch),rk1,name(1:nch),rk2,
     *                name(1:nch),tilt
 300     format('angle_',a,'=',e16.9,' ;',/,'k1_',a,'=',e16.9,' ;',/,
     *         'k2_',a,'=',e16.9,' ;',/,'tilt_',a,'=',e16.9,' ;')
         write(11,101) name(1:nch),rl,name(1:nch),name(1:nch),
     *                 name(1:nch),name(1:nch),tv,d3
 101     format(a,1x,' : RBEND, L=',e16.9,', ANGLE:=angle_',a,
     *        ', K1:=K1_',a,', K2:=K2_',a,', TILT:=tilt_',a,
     *   ', E1=',e16.9,', E2=',e16.9,' ;')         
c 122     format('BEND',2x,'2',2x,e16.9,2x,e16.9,1x,' 0. ',1x,e16.9)
         return
       else
         if(angle.ne.0)then
          rho=0.5*rl/dsin(angle*0.5)
          arc=rho*angle
          e1=angle*0.5
          e2=angle*0.5
         else
          arc=rl
          e1=0
          e2=0
         end if
         write(14,300)name(1:nch),angle,name(1:nch),rk1,name(1:nch),rk2,
     *        name(1:nch),tilt
         write(11,101) name(1:nch),rl,name(1:nch),name(1:nch),
     *                 name(1:nch),name(1:nch),e1,e2         

         
         return
       end if
      end if


      if(type.eq.'QUADRUPOLE')then
         call countc(name,nch)
         write(14,302)name(1:nch),rk1,name(1:nch),tilt
 302     format('k1_',a,'=',e16.9,' ;',/,'tilt_',a,'=',e16.9,' ;')
         write(11,103)name(1:nch),rl,name(1:nch),name(1:nch)
 103     format(a,1x,' : QUADRUPOLE, L=',f10.6,
     *                ', K1:=k1_',a,', TILT:= tilt_',a,' ;')
c 123    format('Q',5x,'3',2x,e16.9,2x,e16.9,1x,' 0. ',1x,' 0.',
c     *         e16.9)
        return
      end if
      if(type.eq.'MONITOR')then
        write(11,104) name,rl
 104    format(a12,1x,' : MONITOR, L=',f10.6,' ;')
        return
      end if
      if(type.eq.'INSTRUMENT')then
        write(11,109) name,rl
 109    format(a12,1x,' : INSTRUMENT, L=',f10.6,' ;')
        return
      end if
      if(type.eq.'HKICKER')then
        if(tilt.ne.0)write(16,200)name,tilt
 200    format(a12,' kick=',f12.10)
        write(11,105) name,rl,tilt
 105    format(a12,1x,' : HKICKER, L=',f10.6,', KICK=',f12.10,' ;')
      write(16,*)"WARNING: mad8 doesn't pass kick values, add manually!"        
        return
      end if
      if(type.eq.'VKICKER')then
        if(tv.ne.0)write(16,200)name,tv
        write(11,106) name,rl,tv
 106    format(a12,1x,' : VKICKER, L=',f10.6,', KICK=',f12.10,' ;')
      write(16,*)"WARNING: mad8 doesn't pass kick values, add manually!"         
        return
      end if
      if(type.eq.'RCOLLIMATOR')then
        write(11,107) name,rl
 107    format(a12,1x,' : RCOLLIMATOR, L=',f10.6,';')
        return
      end if
      if(type.eq.'SEXTUPOLE')then
c        write(11,111) name,rl,rk2
c     111    format(a12,1x,' : SEXTUPOLE, L=',f10.6,', K2=',e16.9,';')
        write(14,119)name,rk2
        write(11,118)name,rl,name
        write(25,119)name,rk2        
        write(25,118)name,rl,name
 118    format(a12,1x,' : SEXTUPOLE, L=',f10.6,', K2:=k2_',a12,';')
 119    format('k2_',a12,'=',e16.9,';')         
        return
      end if
      if(type.eq.'OCTUPOLE')then
c also increasing ORDER mad8 does not write the octupoles values
c into LATTICE.DAT for octupole magnets
         rk3=0
        write(11,117) name,rl,rk3
 117    format(a12,1x,' : OCTUPOLE, L=',f10.6,', K3=',e16.9,';')
        write(16,*)'WARNING: mad8 does not pass K3, add manually!'
        return
      end if      
      if(type.eq.'HMONITOR')then
        write(11,112) name,rl
 112    format(a12,1x,' : HMONITOR, L=',f10.6,' ;')
        return
      end if
      if(type.eq.'VMONITOR')then
        write(11,113) name,rl
 113    format(a12,1x,' : VMONITOR, L=',f10.6,' ;')
        return
      end if
      if(type.eq.'RFCAVITY')then
c in the RR file only the length is specified
c        write(11,114) name,rl,volt,lag,harmon
        write(11,114) name,rl
 114    format(a12,1x,' : RFCAVITY, L=',f10.6,';')
c 114    format(a12,1x,' :RFCAVITY, L=',e16.9,', VOLT=',...,
c     *         ', LAG=',....', HARMON=',...,';')
      write(6,*)"WARNING: RF parameters to be added manually!"         
        return
      end if
      if(type.eq.'KICKER')then
        hkick=0
        vkick=0
        write(11,115) name,rl,hkick,vkick
 115    format(a12,1x,' : KICKER, L=',f10.6,', HKICK=',e16.9,
     *       ', VKICK=',e16.9,' ;')
      write(16,*)"WARNING: mad8 doesn't pass kick values, add manually!"         
        return
      end if



      if(type.eq.'MULTIPOLE'.and.rl.eq.0)then
       if(anss.eq.'y')then
         rksl0=-rksl0
         rksl1=-rksl1
         rksl2=-rksl2
         rksl3=-rksl3
         rksl4=-rksl4
         rksl5=-rksl5
       end if
       dknl0=dabs(rknl0)
       dknl1=dabs(rknl1)
       dknl2=dabs(rknl2)
       dknl3=dabs(rknl3)
       dknl4=dabs(rknl4)
       dknl5=dabs(rknl5)
       dksl0=dabs(rksl0)
       dksl1=dabs(rksl1)
       dksl2=dabs(rksl2)
       dksl3=dabs(rksl3)
       dksl4=dabs(rksl4)
       dksl5=dabs(rksl5)
c       if(ansm.eq.'y')then
c exclude small multipoles
         if(dknl0.lt.delta.and.dknl1.lt.delta.and.dknl2.lt.delta.and.
     *      dknl3.lt.delta.and.dknl4.lt.delta.and.dknl5.lt.delta.and.
     *      dksl0.lt.delta.and.dksl1.lt.delta.and.dksl2.lt.delta.and.
     *      dksl3.lt.delta.and.dksl4.lt.delta.and.dksl5.lt.delta)then
              return
         end if
c       end if
c zeroing small multi coming from rounding errors in lattice.dat
       if(dknl0.lt.delta)rknl0=0
       if(dknl1.lt.delta)rknl1=0
       if(dknl2.lt.delta)rknl2=0
       if(dknl3.lt.delta)rknl3=0
       if(dknl4.lt.delta)rknl4=0
       if(dknl5.lt.delta)rknl5=0
       if(dksl0.lt.delta)rksl0=0
       if(dksl1.lt.delta)rksl1=0
       if(dksl2.lt.delta)rksl2=0
       if(dksl3.lt.delta)rksl3=0 
       if(dksl4.lt.delta)rksl4=0 
       if(dksl5.lt.delta)rksl5=0
       if(ansk.eq.'y')rksl1=0
       write(11,116) name,rknl0,rknl1,rknl2,
     *                     rknl3,rknl4,rknl5,
     *                     rksl0,rksl1,rksl2,
     *                     rksl3,rksl4,rksl5 
       write(22,116) name,rknl0,rknl1,rknl2,
     *                     rknl3,rknl4,rknl5,
     *                     rksl0,rksl1,rksl2,
     *                     rksl3,rksl4,rksl5 

 116   format(a12,1x,' : MULTIPOLE, KNL:={',3(e16.9,','),/,
     *         2(e16.9,','),e16.9,
     *         '},',/,26x,'KSL:={',3(e16.9,','),/,
     *      2(e16.9,','),e16.9,'};')
       
       write(24,220)name,rknl2,rksl2
 220   format(a,2(e16.9,2x))
       
       return
      end if
      write(6,999)type,name
 999  format('Unknown type:',a60,1x,a60)


      return
      end
      subroutine swrite(pos,rl,arc,name,type)
      implicit real*8 (a-h,o-z)
      character*60 name,type
      character*130 buf, text

          write(12,100)name,name,pos-0.5*arc
c increased digit for sodd complain
 100    format(a12,' : ',a12,', at = ',f16.10,' ;')
c        write(16,101)name,type,pos-0.5*arc,arc
c 101    format(a12,' : ',a12,', at s_arc= ',f14.8,' ;!L=',f14.8)

      return
      end

      subroutine check(buf)
      implicit real*8 (a-h,o-z)
      character*60 name,type
      character*130 buf,buff,bufff
c avoid the - pitfall
      ifl=0
      buff=''
      bufff=''
      bufff=buf
      do 1 j=1,95
      if(buf(j+1:j+1).eq.'-'.and.buf(j:j).ne.'E'.and.
     *   buf(j:j).ne.' ')then
        buff(1:j)=buf(1:j)
        buff(j+1:j+1)=' '
        buff(j+2:130)=buf(j+1:130)
        buf=''
        buf=buff
        ifl=1
      end if
 1    continue

      if(ifl.eq.0)then
          buf=''
          buf=bufff
      else
          buf=''
          buf=buff
      end if
      return
      end

      subroutine countc(name,nchar)
      character*60 name
      nchar=0
      do 1 i=1,60
         if(name(i:i).eq.'')return
         nchar=nchar+1
 1    continue
      return
      end
