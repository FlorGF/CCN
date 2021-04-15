program guido 
    integer :: indicador

write(*,*)"ingrese un número del 1 al 5"
read(*,*)indicador 

if(indicador==1)then 
    write(*,*)"popi te ama"
elseif(indicador==2)then 
    write(*,*)"sos el amor de mi vida"
elseif(indicador==3)then 
    write(*,*)"tus besos son los mejores"
elseif(indicador==4)then 
    write(*,*)"cogemos?"
elseif(indicador==5)then 
    write(*,*)"vale por un pt"
else 
    write(*,*)"la pifiaste de numero, ya no me queres"
endif 

end program 