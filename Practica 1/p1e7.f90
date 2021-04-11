subroutine div(a, b)
implicit NONE
integer :: a,b,c
if(a<b) then
  c=mod(b,a)
else
  c=mod(a,b)
end if
if(c==0)then
  write(*,*) "son divisibles"
else
  write(*,*) "no son divisibles"
end if
end subroutine div

program p1e7
  integer :: a, b
  write(*,*) "escriba un numero"
  read(*,*) a
  write(*,*) "escriba otro numero"
  read(*,*) b
  call div(a, b)
end program p1e7
