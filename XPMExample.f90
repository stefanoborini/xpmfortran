program XPMExample
   use XPMModule
   implicit none
   integer :: rgbdata(64,64,3)
   integer :: i,j

   rgbdata = 0
   do i=1, 64
      rgbdata(i,i,1:3) = (/ 255,0,0 /)
      rgbdata(65-i,i,1:3) = (/ 0,0,255 /)
   enddo

   call writeXPM(rgbdata, 6)

end program

