c read madx.def
c read a mad8 input and find out magnet type
c write new def file including type      
c The type is search in the mad8 file when the names
c are identical. It looks also the following line if it
c find a '&'. 
      implicit real*8 (a-h,o-z)
      
      character*60 namefi     
      character*150 textx,text8
      character*14 namex,name8,type


      maxc=150

      write(6,500)
 500  format('Reading:',/,'dummy/madx.def',/,
     *     'created by tomadx',/,
     *     'and a mad8 input',/,
     *     'Writing: dummy/madx_types.def ',/,
     *     'after adding the types')

      write(6,*)'Enter mad8 input'
      read(5,'(a)')namefi

c input
      open(unit=10,file="dummy/madx.def")      
      open(unit=11,file=namefi)
c output
      open(unit=12,file='dummy/madx_types.def')
      open(unit=16,file="tomadx_types.log")

 2    continue
      textx=''
      read(10,'(a)',end=1)textx
      if(textx(1:1).eq.'!'.or.textx(1:1).eq.'#')then
         go to 2
         write(12,'(a)')textx
      end if
      ierr=0
      num=10
      namex=''
      call findn(textx,namex,maxc,ierr,num)
      if(ierr.eq.1)then
c no name found in string, write on file and go to next line
         write(12,'(a)')textx
         go to 2
      end if
      rewind(11)
 4    continue
      text8=''
      read(11,'(a)',end=3)text8
      if(text8(1:1).eq.'!'.or.text8(1:1).eq.'#')go to 4    
      ierr8=0
      num=8
      name8=''
      call findn(text8,name8,maxc,ierr8,num)
c if no name found look next mad8 input line
      if(ierr8.eq.1)go to 4
      if(name8.eq.namex)then
c search for end of line
         ierr=0
         ind=0
         call findch(textx,ind,maxc,ierr)
c  ind is the position of ";"
c      if(ierr.eq.1)then
c         write(12,'(a)')textx
c         go to 2
c      end if
c search for type in mad8 input
      ierrt=0
      call findty(text8,type,maxc,ierrt)
      if(ierrt.eq.1)go to 3
c search for TYPE in the next line
      if(ierrt.eq.2)then
       text8=''
       read(11,'(a)',end=3)text8
       ierrt=0
       call findty(text8,type,maxc,ierrt)
      end if
      if(ierrt.eq.2)then
         write(16,801)namex
 801     format('No TYPE in the first two lines for: ',a)
         go to 2
      end if      
      if(ierrt.eq.1)then
         write(16,800)namex
 800     format('No TYPE for: ',a)
         write(12,'(a)')textx
         go to 2
      end if
      if((ind+20).lt.maxc)write(12,100)textx(1:ind-1),type
      if((ind+20).ge.maxc)write(12,101)textx(1:ind-1),type
 100  format(a,',type=',a,';')
 101  format(a,',',/,'type=',a,';')      
         go to 2
      end if
      go to 4
 3    continue
      if(ierr8.eq.1)write(16,900)namex
      if(ierrt.eq.1)write(16,901)namex      
      write(12,'(a)')textx      
 900  format('No match for: ',a)
 901  format('No TYPE for: ',a)      
      go to 2
 1    continue
      close(10)
      close(11)
      close(12)
      close(16)

      stop
      end
      subroutine findn(text,name,maxc,ierr,num)
      character*150 text
      character*14 name,copy
      copy=''
      name=''

      do 1 i=1,maxc
 800     format('in findn:',i3,1x,a)
         if(text(i:i).eq.':'.and.text(i+1:i+1).ne.'=')then
            copy=text(1:i-1)
            go to 2
         end if
 1    continue
      write(16,900)text
 900  format('No name in string:',a)
      ierr=1
      return
      
 2    continue
      k=0
      do 3 j=1,i-1
         if(copy(j:j).ne.' ')then
            k=k+1
            name(k:k)=copy(j:j)
         end if
 3    continue
      return
      end
      subroutine findch(text,ind,maxc,ierr)   
      character*150 text
      do 1 i=1,maxc
         if(text(i:i).eq.';')then
            ind=i
            return
         end if
 1    continue
      ierr=1
      return
      end
      subroutine findt(text,type,maxc,ierr)   
      character*150 text
      character*14 type
      type=''
      do 1 i=1,maxc
         if(text(i:i+4).eq.'type='.or.text(i:i+4).eq.'TYPE=')then
            knt=0
            do 2 j=i+5,maxc
               if(text(j:j).eq.''.and.knt.eq.0)then
                  knt=knt+1
                  go to 2
               end if
               if((text(j:j).eq.''.and.knt.gt.0).or.
     *           text(j:j).eq.';'.or.text(j:j).eq.',')then
                 type=text(i+5:j-1)
                 return
               end if
 2       continue
         end if
 1    continue
      ierr=1
      return
      end     
      subroutine findty(text,type,maxc,ierr)   
      character*150 text
      character*14 type
      type=''
      do 1 i=1,maxc
         if(text(i:i+3).eq.'type'.or.text(i:i+3).eq.'TYPE')then
            do 3 k=i+4,maxc
            if(text(k:k).eq.'=')then
             knt=0
             do 2 j=k+1,maxc
               if(text(j:j).ne.'')knt=knt+1
               if(text(j:j).eq.''.and.knt.eq.0)then
                  go to 2
               end if
               if((text(j:j).eq.''.and.knt.gt.0).or.
     *           text(j:j).eq.';'.or.text(j:j).eq.',')then
                 type=text(k+1:j-1)
                 return
               end if
 2            continue
           end if
 3         continue
         end if
 1    continue
      
      do 5 i=1,maxc
         if(text(i:i).eq.'&')then
            ierr=2
            return
         end if
 5    continue
      
            
      ierr=1
      return
      end     
