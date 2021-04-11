program p1e10
implicit none 
integer :: n, m, i
real(8), allocatable, dimension(:) :: a, b
real(8) :: aux
open(unit=1,file="datos10.txt",status="old")
read(1,*)a
read(1,*)b
close(unit=1)
n=size(a)
m=size(b)
write(*,*)n, m
!allocate(a(n))
!allocate(b(m))
!if(m==n)then 
   ! do i=1,n
   ! aux=a(i)
   ! a(i)=b(i)
   ! b(i)=aux
   ! enddo
!else
  !  write(*,*)"las dimensiones no son iguales"
!!endif
!write(*,*)a
!!write(*,*)b
!deallocate(a)
!deallocate(b)
end program 