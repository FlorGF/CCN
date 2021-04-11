module splines
    implicit none 
    contains 
    subroutine primer_orden(x, y, y_int, x_int) !spline de primer orden 
        real(8), intent(in) :: x(:), y(:) !los datos 
        real(8), intent(in) :: x_int !el x que quiero interpolar
        real(8), intent(out) :: y_int !el y interpolado 
        integer :: i_int !para indicar en que intervalo esta 
        integer :: i, n 
        n=size(x)
        !chequeo que este bien el x_int 
        if (x_int < x(1) .or. x_int > x(n)) then 
            write(*,*) "Error, x_int está fuera del intervalo de interpolación:"
            stop
        endif 

        !primero tengo que ver en que intervalo esta el x a interpolar, es decir entre que datos esta
        do i=1, n-1
            if(x_int>x(i).and. x_int<x(i+1))then 
                i_int=i 
                exit 
            endif 
        enddo 
        !una vez calculado donde está hacemos la interpolacion 
        y_int=y(i_int)+(x_int-x(i_int))*(y(i_int+1)-y(i_int))/(x(i_int+1)-x(i_int))
    end subroutine 
    !-------------------------------------------------------------------------
    subroutine ordenmenoramayor (x, y) 
        real(8) :: m,s
        integer :: i,n,j
        real(8), intent(inout) :: x(:), y(:)
        logical :: flag
    
    
        flag=.false.
        do i=1,n-1                     !Genera todas las combinaciones de n elementos tomados de 2 en 2
            do j=i+1,n
               if (x(i)>x(j)) then           !si el de la izquierda es mayor, los intercambia
        
                m=x(i)
                s=y(i)
                x(i) = x(j)
                y(i) = y(j)
                x(j) = m
                y(j) = s
        end if
    
        if (i>1 .and. x(i) == x(i-1)) then !se fija si hay una x repetida 
            flag = .true.
        end if
    
        end do
        end do   
    end subroutine 

    !-----------------------------------------------------------------------------------------
    subroutine segundo_orden(x, y, x_int, y_int, deriv_ini)
        real(8), intent(in) :: x(:), y(:) !los datos 
        real(8), intent(in) :: x_int !el x que quiero interpolar
        real(8), intent(out) :: y_int !el y interpolado 
        integer :: i_int !para indicar en que intervalo esta 
        integer :: i, n 
        real(8) :: a(size(x)) !estos párametros son para calcular la interpolacion
        real(8) :: deriv_ini
        n=size(x)
        !chequeo que este bien el x_int 
        if (x_int < x(1) .or. x_int > x(n)) then 
            write(*,*) "Error, x_int está fuera del intervalo de interpolación:"
            stop
        endif 

        !primero tengo que ver en que intervalo esta el x a interpolar, es decir entre que datos esta
        do i=1, n-1
            if(x_int>x(i).and. x_int<x(i+1))then 
                i_int=i 
                exit 
            endif 
        enddo 
        !construyo el y_int
        !a deriv_ini hay que darle un valor inicial 
        do i=1,n-1
            a(i)=( (y(i+1)-y(i)) / (x(i+1)-x(i)) - deriv_ini) / (x(i+1)-x(i))
            deriv_ini = (y(i+1)-y(i)) / (x(i+1)-x(i)) + &
            a(i)*(2.*x(i+1)-(x(i+1)+x(i)))
         enddo

         !una vez obtenido a, y deriv_ini calculo el y interpolado 
         !y_int=y(i_int)+(x_int-x(i_int))*(y(i_int+1)-y(i_int))/(x(i_int+1)-x(i_int))&
         !+a(i_int)*(x-x(i_int))*(x-x(i_int+1))
         y_int=y(i_int) + (x_int-x(i_int))/(x(i_int+1)-x(i_int))*(y(i_int+1)-y(i_int)) + &
         a(i_int) * (x_int-x(i_int)) * (x_int-x(i_int+1))

        end subroutine 

    !----------------------------------------------------------------------------------------
        subroutine spline (x,y,n,yp1,ypn,y2) !yp1 es la derivada primera en x1 e ypn es la derivada primera en xn
            !-----------------------------------------------------------------------
            ! given arrays x(1:n) and y(1:n) containing a tabulated function, i.e.
            ! y_i = f(x_i), with x_1 < x_2 < ... < x_n, and given values yp1 ypn for
            ! the first derivative of the interpolating function at points 1 and n,
            ! this routine returns an array y2(1:n) of length n which contains the
            ! second derivatives of the interpolating function at the tabulated
            ! points x_i. If yp1 and ypn are equal 1.d30 or larger, the routine
            ! is signalled to set the corresponding boundary conditions for a
            ! natural spline, with zero second derivative on that boundary.
            ! (c) modified from Numerical recipes
            !-----------------------------------------------------------------------
            implicit none
            integer :: n,i,k
            integer, parameter :: idouble = kind(1.0d0)
            integer, parameter :: isingle = kind(1.0)
            real(idouble), dimension(n), intent(in) :: x,y
            real(idouble), dimension(n), intent(out) :: y2
            real(idouble), dimension(n) :: u
            real(idouble), intent(in) :: yp1,ypn
            real(idouble) :: p,qn,sig,un
    
            if (yp1.gt..99e30) then
                y2(1) = 0.
                u(1) = 0.
                else
                y2(1) = -0.5
                u(1) = (3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
                endif
                do i = 2,n-1
                sig = (x(i)-x(i-1))/(x(i+1)-x(i-1))
                p = sig*y2(i-1)+2.
                y2(i) = (sig-1.)/p
                u(i) = (6.*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1)) &
                & /(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*u(i-1))/p
                enddo
                if (ypn.gt..99e30) then
                qn = 0.
                un = 0.
                else
                qn = 0.5
                un = (3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
                endif
                y2(n) = (un-qn*u(n-1))/(qn*y2(n-1)+1.)
                do k = n-1,1,-1
                y2(k) = y2(k)*y2(k+1)+u(k)
                enddo
                return
                end subroutine 
    
                !--------------------------------------------------------------------------------------------------"
    
                SUBROUTINE tercer_orden(xa, ya, y2a, n, x, y)
                    ! USE nrtype
                    !!
                    !Given the arrays xa(1:n) and ya(1:n) of length n, which tabulate a function
                    ! (with the xa(i) in order), and given the array y2a(1:n), which is the output
                    ! from the subroutine spline, and given a value of x, this routine returns a
                    ! cubic spline interpolated value y.
                    ! Adapted from Numerical Recipes in FORTRAN 77
                    !
                    IMPLICIT NONE
                    INTEGER, PARAMETER :: DP = KIND(1.0D0)
                    INTEGER :: n
                    REAL(DP) :: x, y, xa(n), y2a(n), ya(n)
                    INTEGER :: k, khi, klo
                    REAL(DP) :: a, b, h
                    klo=1
                    khi=n
                    1 if (khi-klo.gt.1) then
                    k=(khi+klo)/2
                    if (xa(k).gt.x) then
                    khi=k
                    else
                    klo=k
                    endif
                    goto 1
                    endif
                    h=xa(khi)-xa(klo)
                    if (h.eq.0.) then
                    write(*,*) 'bad xa input in splint'
                    stop
                    endif
                    a=(xa(khi)-x)/h
                    b=(x-xa(klo))/h
                    y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**2)/6.
                    return
                END SUBROUTINE 




end module