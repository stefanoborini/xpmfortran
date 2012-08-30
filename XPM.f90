module XPMModule
! quick and dirty XPM drawing module for student exercises in fortran.
! MIT license. Author Stefano Borini
   implicit none
   private
   save
   public :: WriteXPM
   character(len=1), parameter :: INDEX_DIGITS(64) = (/ '+','.','*','1','2','3','4','5', &
                                                        '6','7','8','9','0','a','b','c', &
                                                        'd','e','f','g','h','i','j','k', &
                                                        'l','m','n','o','p','q','r','s', &
                                                        't','u','v','w','x','y','z','A', &
                                                        'B','C','D','E','F','G','H','I', &
                                                        'J','K','L','M','N','O','P','Q', &
                                                        'R','S','T','U','V','W','X','Y' /)
contains
   
   subroutine WriteXPM(rgbdata, unit)
      integer, intent(in) :: rgbdata(:,:,:)
      integer, intent(in) :: unit

      integer :: colorTable(256**3,3), numColors, colorIdx, row, col, numColorChars
      character(len=10) :: widthString, heightString, numColorsString, numColorCharsString
      character(len=4) :: colorString

      call CreateColorTable(rgbdata, colorTable, numColors)
      
      write (unit, '(A)') '/* XPM */'
      write (unit, '(A)') 'static char * image[] = {'

      write(widthString,"(I10)") size(rgbdata,2)
      write(heightString,"(I10)") size(rgbdata,1)
      write(numColorsString,"(I10)") numColors
      numColorChars = len_trim(ColorIndexToString(numColors))
      write(numColorCharsString,"(I10)") numColorChars
      
   
      write (unit, "(A)") '"'//trim(adjustl(widthString))//" "// &
                           trim(adjustl(heightString))//" "// &
                           trim(adjustl(numColorsString))//" "// &
                           trim(adjustl(numColorCharsString))//'",'
     
      do colorIdx=1, numColors
         write (unit, "(A)") '"'//ColorIndexToString(colorIdx)//" c "//RGBToHexString(colorTable(colorIdx,1:3))//'",'
      enddo 

      do row=1, size(rgbdata,1)
         write (unit, "(A)", advance="NO") '"'
         do col=1, size(rgbdata,2)
            colorString = ColorIndexToString( FindColorIndex(colorTable,numColors,rgbdata(row, col, 1:3)))
            write (unit, "(A)", advance="NO") colorString(1:numColorChars)
         enddo
         if (row /= size(rgbdata,1)) then
            write (unit, "(A)") '",'
         else
            write (unit, "(A)") '"'
         endif
      enddo
      write(unit, "(A)"), "};"

   end subroutine
   function FindColorIndex(colorTable, numColors, rgb)
      integer, intent(in) :: colorTable(:,:)
      integer, intent(in) :: numColors
      integer, intent(in) :: rgb(:)
      integer :: FindColorIndex
      integer :: i
      do i=1, numColors
         if (all(colorTable(i,1:3) == rgb(1:3) )) then
            FindColorIndex = i
            return
         endif
      enddo

      print *, "Unexpected conditions in FindColorIndex"
      stop
      
   end function
   subroutine CreateColorTable(rgbdata, colorTable, numColors)
      integer, intent(in)  :: rgbdata(:,:,:)
      integer, intent(out) :: colorTable(:,:)
      integer, intent(out) :: numColors 
      integer :: row, col
      logical :: found
      integer :: colorIdx

      numColors = 0
      do row=1, size(rgbdata,1)
         do col=1, size(rgbdata,2)
            found = .false.
            do colorIdx=1, numColors
               if (all(colorTable(colorIdx,1:3) == rgbdata(row,col,1:3))) then
                  found = .true.
                  exit 
               endif
            enddo

            if (.not. found) then
               numColors = numColors+1
               if (numColors > size(colorTable)) then
                  print *, "too many colors"
                  stop
               endif
               colorTable(numColors,1:3) = rgbdata(row,col,1:3)
            endif
         enddo
      enddo
   end subroutine
   function ColorIndexToString(index)
      integer, intent(in) :: index
      character(len=4) :: ColorIndexToString
      integer :: i, idx, m

      idx = index
      ColorIndexToString='    '
      do i=1,4
         m = mod(idx,64)
         ColorIndexToString(i:i)= INDEX_DIGITS(m+1)
         idx = idx/64
         if (idx == 0) exit
      enddo
   end function

   function RGBToHexString(rgb)
      integer, intent(in) :: rgb(3)
      character(len=7) :: RGBToHexString

      RGBToHexString = "#"//hex(rgb(1))//hex(rgb(2))//hex(rgb(3)) 
   end function
   function hex(byte)
      integer, intent(in) :: byte
      character(len=2) :: hex
      character(len=1), parameter :: HEX_DIGITS(0:15) = (/ '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F' /)
      integer :: m1, m2

      m1 = mod(byte,16)
      m2 = mod(byte/16,16)
      hex = HEX_DIGITS(m2)//HEX_DIGITS(m1)
      
   end function

   
end module
