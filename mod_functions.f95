module mod_functions
    implicit none
    
    !!!! DATA
    ! a structure to contain one polygon
    type :: polygon
        integer :: numberOfEdges = 0
        real :: z1 = 0, z2 = 0
        real, dimension(:), allocatable :: x, y
    end type polygon
    
    
    
    ! a structure to contain all relevant varables
    type :: control_struct
        real, allocatable, dimension(:) :: profil_vector_1D
        real :: depth, radius, rho, length_1D, tick_1D
        integer :: numberOfEdges, numberOfLayers, vector_length_1D
    end type control_struct
        
    
    
    !!!! FUNCTIONS
    contains


	real function round_6(input)
		real :: input, temp
		
		temp = input*100000
		round_6  = anint(temp) / 100000
		return
	
	end function round_6
    
    
    
    subroutine calc_profil_vector(config)
    type(control_struct), intent(inout) :: config
    integer :: i
    
    ! calculate the length of the vector
    config%vector_length_1D = int(config%length_1D/config%tick_1D)+1
    
    ! allocate the new vector with the length from above
    allocate(config%profil_vector_1D(config%vector_length_1D))
    
    ! set starting point of vector
    config%profil_vector_1D(1) = -(abs(config%length_1D)/2)
    
    ! fill vector with elements
    do i=2, config%vector_length_1D
        config%profil_vector_1D(i) = config%profil_vector_1D(i-1) + config%tick_1D
    end do
   
    end subroutine
    
    
    ! a function to read the config-file
    subroutine read_config ( configurations )
        implicit none
        type(control_struct), intent(inout) :: configurations
        real :: depth, radius, rho, length_1D, tick_1D
        integer :: numberOfEdges, numberOfLayers
        integer :: iostatus = 0
        
        ! Deklaration der Namelist des Config-files
        NAMELIST /conf/ rho, depth, radius, numberOfEdges, numberOfLayers, length_1D, tick_1D
        
        
        ! Oeffnen des Config-files
        open(10,file='config.nml',delim='apostrophe',status='old',action='read',iostat=iostatus)
        if(iostatus.ne.0) stop ": ERROR opening config-file"
        
        ! Einlesen des Config-files
        read(10,nml=conf,iostat=iostatus)
        if(iostatus.ne.0) stop ": ERROR reading config-file"
        
        close(10)
        
        configurations%rho = rho
        configurations%depth = depth
        configurations%radius = radius
        configurations%numberOfEdges = numberOfEdges
        configurations%length_1D = length_1D
        configurations%tick_1D = tick_1D
        configurations%numberOfLayers = numberOfLayers
        
        return
    end subroutine read_config
    
    
    
    subroutine create_sphere (config, poly)
        ! returns x, y, z, vectors which contain the coordinates of every edge of the polygon clockwise
    
    type(control_struct), intent(inout) :: config
    type(polygon), allocatable, dimension(:), intent(out) :: poly
        
    integer :: status = 0, i, k
    real :: pi, angle_edge, angle_layer, radius_layer, diameter, layerThickness, z, top_depth
    
    ! allocation of all polygons
    allocate(poly(config%numberOfLayers), stat=status)
    if (status .ne. 0) stop 'ALLOCATION OF POLYGON-STRUCT FAILED'
    do i=1,config%numberOfLayers
        ! +1 because the first corner is equal to the last corner
        allocate(poly(i)%x(config%numberOfEdges), stat=status)
        if (status .ne. 0) stop 'ALLOCATION OF X FAILED'
        
        allocate(poly(i)%y(config%numberOfEdges), stat=status)
        if (status .ne. 0) stop 'ALLOCATION OF Y FAILED'
    end do

    pi = 4.*atan(1.)
    
    angle_edge = (2. * pi) / config%numberOfEdges
    angle_layer = pi / config%numberOfLayers
    
    !loop over prisms 
    do k = 1, config%numberOfLayers
    
        ! calc radius for each layer!
        radius_layer = abs(sin(angle_layer * (k-0.5)) * config%radius)
        !print*,radius_layer
        
        
        ! calc upper (z1) and lower(z2) side of the polygon Eigentlich geiler wenn ausgelagert
        diameter = config%radius * 2
        layerThickness = diameter / config%numberOfLayers
        

        !calc depth of the top of the shpere
        top_depth = config%depth - config%radius
        
        z = (k-0.5)*layerthickness + top_depth

        
        poly(k)%z1 = z - layerThickness / 2 
        poly(k)%z2 = z + layerThickness / 2 

        ! loop over corners
        do i=1, config%numberOfEdges
            
            poly(k)%x(i) = sin(i*angle_edge)*radius_layer
            poly(k)%y(i) = cos(i*angle_edge)*radius_layer
            
        end do
        

    end do

    ! later: maybe dynamic number of edges... (fewer edges for small polygons...)
    poly%numberOfEdges = config%numberOfEdges
    
    return
    end subroutine create_sphere
    
    
    
    
    
    subroutine output_one_prism (config,poly )
        integer :: j, stat10
        type(control_struct), intent(in) :: config
        type(polygon), allocatable, dimension(:), intent(in) :: poly
        
        open(10, file='output_1_prism.dat', form='formatted', action='write', iostat=stat10)
        if(stat10 .ne. 0) stop 'OPEN output_1_prism.dat FAILED'
        
        do j = 1 , config%numberOfEdges+1
            write(10,'(F12.9,x,F12.9,x,F12.9)') poly(4)%x(j),poly(4)%y(j),poly(4)%z1
        end do
        
        do j = 1, config%numberOfEdges+1
            write(10,'(F12.9,x,F12.9,x,F12.9)') poly(4)%x(j),poly(4)%y(j),poly(4)%z2
        end do
        
        close(10)
    end subroutine output_one_prism
    
    
    subroutine output_sphere (config,poly )
        integer :: i, j, stat10
        type(control_struct), intent(in) :: config
        type(polygon), allocatable, dimension(:), intent(in) :: poly
        
        open(10, file='output_sphere.dat', form='formatted', action='write', iostat=stat10)
        if(stat10 .ne. 0) stop 'OPEN output_sphere.dat FAILED'
        
        do i = 2, config%numberOfLayers-1
            do j = 1 , config%numberOfEdges+1
                write(10,'(F12.9,x,F12.9,x,F12.9)') poly(i)%x(j),poly(i)%y(j),poly(i)%z1
            end do
            
            do j = 1, config%numberOfEdges+1
                write(10,'(F12.9,x,F12.9,x,F12.9)') poly(i)%x(j),poly(i)%y(j),poly(i)%z2
            end do
        end do
        
        close(10)
    end subroutine output_sphere
    
    
    
    
    subroutine profil_1d_x(config,poly)
    type(control_struct), intent(in) :: config
    type(polygon), allocatable, dimension(:), intent(in) :: poly
    real, allocatable, dimension(:) :: profil_1d, grav_anal
    integer :: i

    
    allocate(profil_1d(config%vector_length_1D))
    allocate(grav_anal(config%vector_length_1D))

    ! schleife ueber sum_grav
    do i = 1, config%vector_length_1D
		    
        call sum_grav(config, poly, profil_1d(i), config%profil_vector_1D(i),0.)
        call anal_solution(config, config%profil_vector_1D(i),grav_anal(i))
    end do
	 call save_profile_1d(config, profil_1d, grav_anal)
    end subroutine profil_1d_x



    subroutine calc_grav_polygon(config,poly, gravi_poly, displacement_x, displacement_y)
        type(control_struct), intent(in) :: config
        type(polygon), intent(in) :: poly
        real, intent(in), optional :: displacement_x, displacement_y
        real, intent(out) :: gravi_poly
        real, dimension(:), allocatable :: x, y, rk, S, dx, dy, ds,  d1, d2, C, Rk1, Rk2, P, A, summe
        real, parameter :: G = 6.67384e-11
        integer :: edge,nextEdge, nEdges, sp

        nEdges = config%numberOfEdges

        allocate(dx(nEdges))
        allocate(dy(nEdges))
        allocate(ds(nEdges))
        allocate(d1(nEdges))
        allocate(d2(nEdges))
        allocate(rk(nEdges))
        allocate(S(nEdges))
        allocate(C(nEdges))
        allocate(P(nEdges))
        allocate(Rk1(nEdges))
        allocate(Rk2(nEdges))
        allocate(summe(nEdges))
        allocate(x(nEdges))
        allocate(y(nEdges)) 
        allocate(A(nEdges))
              
        summe = 0.
        
        x = displacement_x + poly%x
        y = displacement_y + poly%y
        
        
        
        ! calc rk
        do edge = 1,nEdges
            rk(edge) = sqrt(x(edge)**2 + y(edge)**2)
            !print*, rk(edge)
        end do


        ! Calc delta x, delta y
        do edge = 1, nEdges
            if(edge .eq. nEdges) then
                nextEdge = 1
            else
                nextEdge = edge + 1
            end if
            
            dx(edge) = x(nextEdge) - x(edge)
            dy(edge) = y(nextEdge) - y(edge)
            ds(edge) = sqrt(dx(edge)**2 + dy(edge)**2)
            P(edge) = ((x(edge)*y(nextEdge)) - (x(nextEdge)*y(edge)))/ds(edge)
            S(edge) = dx(edge)/ds(edge)
            C(edge) = dy(edge)/ds(edge)
            Rk1(edge) = sqrt(rk(edge)**2 + poly%z1**2)
            Rk2(edge) = sqrt(rk(edge)**2 + poly%z2**2)
            d1(edge) = (x(edge)*S(edge)) + (y(edge)*C(edge))
            d2(edge) = (x(nextEdge)*S(edge)) + (y(nextEdge)*C(edge))
            A(edge) = acos(((x(edge)*x(nextEdge)) + (y(edge)*y(nextEdge)))/(rk(edge)*rk(nextEdge)))
            
            ! due to precision problems 
			if(isnan(A(edge))) A(edge) = 0

        end do

        
        do edge = 1, nEdges
            if(edge .eq. nEdges) then
                nextEdge = 1
            else
                nextEdge = edge + 1
            end if

            if (P(edge) .gt. 0) then
                sp = 1
                summe(edge) = sp * A(edge) * (poly%z2 - poly%z1) &
                    + poly%z2*( atan((poly%z2 * d1(edge)) / (P(edge) * Rk2(edge))) - &
                      atan((poly%z2 * d2(edge)) / (P(edge) * Rk2(nextEdge)))) &
                    - poly%z1*( atan((poly%z1 * d1(edge)) / (P(edge) * Rk1(edge))) - &
                      atan((poly%z1 * d2(edge)) / (P(edge) * Rk1(nextEdge)))) &
                    - P(edge) * log( ((Rk2(nextEdge) +d2(edge))/ (Rk2(edge) + d1(edge))) * &
                      ((Rk1(edge) + d1(edge)) / (Rk1(nextEdge) + d2(edge))) )
                    
            else if (P(edge) .lt. 0) then
                sp = -1
                summe(edge) = sp * A(edge) * (poly%z2 - poly%z1) &
                    + poly%z2*( atan((poly%z2 * d1(edge)) / (P(edge) * Rk2(edge))) - &
                      atan((poly%z2 * d2(edge)) / (P(edge) * Rk2(nextEdge)))) &
                    - poly%z1*( atan((poly%z1 * d1(edge)) / (P(edge) * Rk1(edge))) - &
                      atan((poly%z1 * d2(edge)) / (P(edge) * Rk1(nextEdge)))) &
                    - P(edge) * log( ((Rk2(nextEdge) +d2(edge))/ (Rk2(edge) + d1(edge))) * &
                      ((Rk1(edge) + d1(edge)) / (Rk1(nextEdge) + d2(edge))) )
                    
            else
                summe(edge) = 0
            end if

            
 
        end do
        ! calc in mGal
        gravi_poly = config%rho * G * sum(summe)*1.e5 
        

        deallocate(dx)
        deallocate(dy)
        deallocate(ds)
        deallocate(d1)
        deallocate(d2)
        deallocate(rk)
        deallocate(S)
        deallocate(C)
        deallocate(P)
        deallocate(Rk1)
        deallocate(Rk2)
        deallocate(summe)
        deallocate(x)
        deallocate(y)
        deallocate(A)
    end subroutine calc_grav_polygon
    
    
    
    subroutine sum_grav(config, poly, grav_body, displacement_x,displacement_y)
        type(control_struct), intent(in) :: config
        type(polygon), allocatable, dimension(:), intent(in) :: poly
        real, intent(in),optional :: displacement_x, displacement_y
        real, intent(out) :: grav_body
        integer :: pIndex
        real, allocatable, dimension(:) :: grav_poly

         !!!!DEALLOCATE EVERYWHERE
        allocate(grav_poly(config%numberOfLayers))
        
        
        do pIndex = 1, config%numberOfLayers
            call calc_grav_polygon(config, poly(pIndex), grav_poly(pIndex),displacement_x,displacement_y)
        end do
        
        grav_body = sum(grav_poly)

        deallocate(grav_poly)
    end subroutine sum_grav
    
    
    
    subroutine save_profile_1d(config,profil_1d,grav_anal)
    type(control_struct), intent(in) :: config
    real, dimension(:), intent(in) :: profil_1d, grav_anal
    integer :: i,stat10, stat11
    
    open(11, file='profil_1D_anal.dat', form='formatted', action='write', iostat=stat11)
    if(stat11 .ne. 0) stop 'OPEN profil_1D_anal.dat FAILED'
    open(10, file='profil_1D.dat', form='formatted', action='write', iostat=stat10)
    if(stat10 .ne. 0) stop 'OPEN profil_1D.dat FAILED'

    do i = 1, config%vector_length_1D
        write(10,'(F13.8,x,F13.9)') config%profil_vector_1D(i), profil_1d(i)
        write(11,'(F13.8,x,F13.9)') config%profil_vector_1D(i), grav_anal(i)
    end do
    
    close(10)
    close(11)
    
    end subroutine save_profile_1d
    
    
    	subroutine test_numberOfEdges(config)
		type(control_struct), intent(inout) :: config
		
		! create analytical solution
		
		! open file
		
		! loop over numberOfEdges
		
			! Allocate polygon
		
			! calculate gravimetrical anomaly
		
			! compare to analytical solution
		
			! save differences to file (numberOfEdges	difference)
		
			! deallocate polygon
		
		! end loop
		
		! close file
		
	end subroutine test_numberOfEdges



	subroutine test_numberOfLayers(config)
		type(control_struct), intent(inout) :: config
		
		! create analytical solution
		
		! open file
		
		! loop over numberOfLayers
		
			! Allocate polygon
		
			! calculate gravimetrical anomaly
		
			! compare to analytical solution
		
			! save differences to file (numberOfLayers	difference)
		
			! deallocate polygon
		
		! end loop
		
		! close file
		
	end subroutine test_numberOfLayers
    
    
    
	subroutine anal_solution(config, displacement_x, gravi_anal)
		! after Blakely: Gravity and Magnatic Applications
		
		type(control_struct), intent(in) :: config
		real, intent(in) :: displacement_x
		real, intent(out) :: gravi_anal
		real :: mass, pi, rx, rz, r
		real, parameter :: G = 6.67384e-11
		pi = 4.*atan(1.)
		
		mass = (4./3.) * pi * config%rho * config%radius**3 
		
		rx = displacement_x - 0.
		rz = 0. - config%depth
		
		r = sqrt( rx**2 + rz**2 )
		
		gravi_anal = -((G * mass) / (r**2)) * 10e5

    end subroutine anal_solution
    
    
end module mod_functions

