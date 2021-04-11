subroutine localizacion (x, y, region)
    real, intent(in) :: x, y
    real, intent(out) :: region
    real :: r
    r=sqrt(x**2+y**2)
    

    !busco la region donde esta el punto
    if(r>0.75)then !estoy fuera del circulo
        if(abs(x)<=1 .and. abs(y)<=1) then !dentro del cuadrado con limites x=+-1 y=+-1
            if(x>0 .and. y>0)then !primer cuadrante
                region=2
            elseif(x>0 .and. y<0)then !cuarto cuadrante
                region=3
            elseif(x<0 .and. y>0)then !segundo cuadrante
                region=4
            elseif(y==0 .and. x<0)then !eje x negativo
                region=4
            elseif(y==0 .and. x>0)then !frontera entre R2 y R3
                region=5
            elseif(x==0 .and. y>0)then !frontera entre R2 y R4
                region=5
            elseif(x<0 .and. y<0)then !tercer cuadrante
                region=6
            elseif(x==0 .and. y<0)then 
                region=3
        
            endif 
        else 
            region=6
        endif 
    elseif(r<0.75)then !dentro del circulo
        region=1
    elseif(r==0.75)then 
        if(x<0 .and. y<0)then
        region=1
        else 
            region=6
        endif 

    endif 
    
   
end subroutine 
