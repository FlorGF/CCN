program p1e14
implicit none 
real(8),allocatable,dimension(:) :: a, b, c
integer :: cont, iostatus, i
real(8) :: aux
open(unit=1,file="datos14.txt",status="old")
cont=0
iostatus=0
do while(iostatus==0)
    read(1,*,iostat=iostatus)aux
    if(iostatus==0) cont=cont+1
    if(iostatus>0) then
        write(*,*)"Error"
        stop
    elseif(iostatus<0)then
        exit 
    endif
enddo
write(*,*)"La dimensión es:"
write(*,*)cont
allocate(a(cont))
allocate(b(cont))
allocate(c(cont))
rewind(unit=1)
do i=1,cont
    read(1,*)a(i), b(i)
    c(i)=a(i)+b(i)
enddo
write(*,*)a
write(*,*)b
write(*,*)c
deallocate(a)
deallocate(b)
deallocate(c)

close(unit=1)
end program 
