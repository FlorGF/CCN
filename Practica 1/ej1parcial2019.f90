program ej1parcial2019
    real :: x, y, region
!pido que ingresen los puntos
    write(*,*)"Ingrese el punto (x;y)"
    read(*,*)x, y
    write(*,*)"El radio es"
    write(*,*)sqrt(x**2+y**2)
!llamo a la subrutina
    call localizacion(x, y, region)
!me fijo en que region esta
    if(region==1)then
        write(*,*)"Esta en la region 1"
    elseif(region==2)then
        write(*,*)"Esta en la region 2"
     elseif(region==3)then
        write(*,*)"Esta en la region 3"
    elseif(region==4)then
        write(*,*)"Esta en la region 4"
     elseif(region==5)then 
         write(*,*)"Esta en la frontera"
     elseif(region==6)then 
        write(*,*)"No esta en ninguna region determinada"
     endif  
    end program 
    
