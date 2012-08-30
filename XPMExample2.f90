program XPMExample
   use XPMModule
   implicit none
   integer :: rgbdata(256,256,3)
   integer :: i,j

   do i=1, 256
      do j=1,256
         rgbdata(i,j,1:3) = (/ i-1,j-1,0 /)
      enddo
   enddo

   call writeXPM(rgbdata, 6)

end program

