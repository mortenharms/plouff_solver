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
		real :: depth, radius, rho, G
		integer :: numberOfEdges, numberOfLayers
	end type control_struct
		
	


	
	
	
	!!!! FUNCTIONS
	contains
	
	! a function to read the config-file
	subroutine read_config ( configurations )
		implicit none
		type(control_struct), intent(inout) :: configurations
		real :: depth, radius
		integer :: numberOfEdges, numberOfLayers
		integer :: iostatus = 0
		
		! Deklaration der Namelist des Config-files
		NAMELIST /conf/ depth, radius, numberOfEdges, numberOfLayers
		
		
		! Oeffnen des Config-files
		open(10,file='config.nml',delim='apostrophe',status='old',action='read',iostat=iostatus)
		if(iostatus.ne.0) stop ": ERROR opening config-file"
		
		! Einlesen des Config-files
		read(10,nml=conf,iostat=iostatus)
		if(iostatus.ne.0) stop ": ERROR reading config-file"
		
		close(10)
		
		configurations%depth = depth
		configurations%radius = radius
		configurations%numberOfEdges = numberOfEdges
		
		!@FIXME TEST IF numberOfLayers is odd
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
		allocate(poly(i)%x(config%numberOfEdges + 1), stat=status)
		if (status .ne. 0) stop 'ALLOCATION OF X FAILED'
		
		allocate(poly(i)%y(config%numberOfEdges + 1), stat=status)
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
		
		
		! calc depth for each layer
		!z = sin(angle_layer * (k-1)) * config%radius
		!calc depth of the top of the shpere
		top_depth = config%depth - config%radius
		
		z = (k-0.5)*layerthickness + top_depth
		!print*,z
		
		poly(k)%z1 = z - layerThickness / 2 
		poly(k)%z2 = z + layerThickness / 2 
		!print*,'z1=',poly(k)%z1,'z2=',poly(k)%z2
	
		! loop over corners
		do i=1, config%numberOfEdges
			! deg to rad -> *pi/180
			!poly(k)%x(i+1) = sin(i*angle_edge*pi/180)*radius_layer
			!poly(k)%y(i+1) = cos(i*angle_edge*pi/180)*radius_layer
			poly(k)%x(i) = sin(i*angle_edge)*radius_layer
			poly(k)%y(i) = cos(i*angle_edge)*radius_layer
			
		end do
		
		
		! the first corner ist equal to the last corner
		poly(k)%x(config%numberOfEdges+1) = poly(k)%x(1)
		poly(k)%y(config%numberOfEdges+1) = poly(k)%y(1)
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
	
	
	
	
	subroutine calc_A( )
	
	end subroutine calc_A
	
	
	subroutine sum_grav(config, poly)
		type(control_struct), intent(in) :: config
		type(polygon), allocatable, dimension(:), intent(in) :: poly


	end subroutine sum_grav

		
	! GRAV_PRISM
	subroutine calc_grav_polygon(config,poly, gravi_poly, displacement_x, displacement_y)
		type(control_struct), intent(in) :: config
		type(polygon), intent(in) :: poly
		real, intent(in), optional :: displacement_x, displacement_y
		real, intent(out) :: gravi_poly
		real, dimension(:), allocatable :: x, y, r, S, dx, dy, ds,  d1, d2, C, Rk1, Rk2, P, A, summe
		integer :: edge, nEdges, sm, sp
		
		nEdges = config%numberOfEdges
		x = displacement_x + poly%x
		y = displacement_y + poly%y
		summe = 0
		
		allocate(dx(nEdges))
		allocate(dy(nEdges))
		allocate(ds(nEdges))
		allocate(r(nEdges))
		allocate(S(nEdges))
		allocate(C(nEdges))
		allocate(P(nEdges))
		allocate(Rk1(nEdges))
		allocate(Rk2(nEdges))
		allocate(summe(nEdges))
		
		! calc r 
		r(:) = sqrt(x(:)**2 + y(:)**2)
		
		! calc S
		
		S(:) = dx(:)/ds(:)
		! S = dx/dy
		! S(1:nEdges) = dx(1:nEdges) / ds(1:nEdges)
		
		! Calc delta x, delta y
		do edge = 1, nEdges
			dx(edge) = x(edge+1) - x(edge)
			dy(edge) = y(edge+1) - y(edge)
		end do
		
		! calc delta S
		ds(1:nEdges)=sqrt(dx(1:nEdges)**2 + dy(1:nEdges)**2)
		
		! calc C
		C(1:nEdges) = dy(1:nEdges)/ds(1:nEdges)
		
		! calc P
		P(1:nEdges) = x(1:nEdges)*C(1:nEdges) - y(1:nEdges)*S(1:nEdges)
		
		!Calc Rk1, Rk2
		Rk1(1:nEdges) = r(1:nEdges)**2 + poly%z1**2
		Rk2(1:nEdges) = r(1:nEdges)**2 + poly%z2**2
		
		! calc d1, d2
		do edge = 1, nEdges
			d1(edge) = x(edge)*S(edge) + y(edge)*C(edge)
			d2(edge) = x(edge+1)*S(edge) + y(edge+1)*C(edge)
			A(edge) = acos((y(edge)*y(edge+1) - y(edge)*y(edge+1))/(r(edge)*r(edge+1)))
		end do
		
		
		do edge = 1, nEdges

			 if (P(edge) .lt. 0) then
				 sp = -1
			else if (P(edge) .gt. 0) THEN
				sp = 1
			else
				! Gravi = 0!!!!!
			end if
			
			summe(edge)	= sp * A(edge) * (poly%z1 - poly%z2) &
					+ poly%z2*( atan((poly%z2*d1(edge))/(P(edge)*Rk2(edge))) - atan((poly%z2*d2(edge))/(P(edge)*Rk2(edge+1)))) &
					+ poly%z1*( atan((poly%z1*d1(edge))/(P(edge)*Rk1(edge))) - atan((poly%z1*d2(edge))/(P(edge)*Rk1(edge+1)))) &
					- P(edge) * log( ((Rk2(edge+1)+d2(edge))/(Rk2(edge)+d1(edge)))*((Rk1(edge)+d2(edge))/(Rk2(edge)+d2(edge))) )
					
			
		end do
		
		gravi_poly = config%rho * config%G * sum(summe)

	end subroutine calc_grav_polygon
	
	
	
	
	
	subroutine calc_analytical_solution( )
		
	end subroutine calc_analytical_solution
	
	
end module mod_functions

