subroutine bomb(fila, columna, cbombas, minas)
    implicit none ! comienza el programa 
 
    real ::  u(2500), v(2500) ! vectores generadores de numeros aleatorios  que se convertiran en fila y columna 
    integer :: i, j, k, l,  contu, contv, bombas ! i,j,k,l subíndices, contu contv contadores de los vectores
    integer :: fila, columna, cbombas
    character ::  minas(1000,1000), charbombas !matriz del buscaminas, y la variable character de bombas
 
    10 format(i1) ! formato para leer las bombas
    contu = 1; contv = 1; bombas = 0   ! contadores inicialmente
 ! generamos dos tandas de numeros aleatorios para  
    call random_number(v) 
    call random_number(u) 
 !creamos una matriz vacía muy grande para que no de problemas 
    do i = 1, 1000
 
       do j = 1, 1000
 
          minas(i,j) = " " 
 
       end do 
 
    end do 
 
    do    !creamos un bucle hasta que las bombas coincidan con las solicitadas por el usuario
       do ! comprobamos si en nuestros vectores de numeros aleatorio hay un 0, si lo hay pasamos al siguiente
          !valor  en la fila o en la columna, hasta que los dos sean distintos de 0 
 
          if(int((fila+1)*u(contu))==0)then
 
             contu = contu + 1
 
          elseif(int((columna+1)*v(contv))==0)then 
 
             contv = contv + 1
 
          end if
          if((int((fila+1)*u(contu))/=0).and.(int((columna+1)*v(contv))/=0))exit! cuandoo son distinto de 0 sale del comprobador
       end do 
 
       if(minas(int((fila+1)*u(contu)),int((columna+1)*v(contv)))==" ")then ! si hay un espacio en blanco en la casilla aleatoria
          !se le asignara una bomba que se identificará como x 
 
          minas(int((fila+1)*u(contu)),int((columna+1)*v(contv)))= "x"
          bombas = bombas + 1 ! contador de cuantas bombas hay 
 
       end if 
 
       contu = contu + 1 ! los contadores cambian 
       contv = contv + 1 
 
       if(bombas == cbombas)exit!si la cantidad de bombas es la que pide el usuario se termina el bucle 
 
    end do 
 
 !hasta aquí ha sido solo para asignar las bombas , ahora habrá que asignar a cada poscion de la matriz solicitada cuantas
 !bombas hay alrededor de esa posicion
 do i = 1, fila
 
    do j = 1, columna
 
       if(minas(i,j)/="x")then! tenemos que ignorar  los lugares donde haya x para que no sobreescriba
    
          bombas = 0  ! empezamos a contar cuantas bombas hay en cada posicion
    
          do k = i-1, i+1
    
             do l = j-1, j+1 ! para esto hay que mirar la matriz 3x3 siendo i,j la posicion central 
    
                if(minas(k,l)=="x")then  ! contador 
    
                   bombas = bombas + 1 
    
                end if 
    
             end do
    
          end do 
 
          write(charbombas,10)bombas ! cambiamos la variable a las bombas 
 
          if(charbombas/="0")then !no queremos que las casillas vacias pongan 0
 
             minas(i,j) = charbombas
 
          end if 
 
       end if 
    
    end do 
 
 end do 
 
 end subroutine 