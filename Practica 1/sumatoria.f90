integer(8) function sumatoria(m,n)
implicit none   
integer(8):: m, n, i, s, j

s=0
do i=m, n
s=s+i
enddo
sumatoria=s
end function sumatoria