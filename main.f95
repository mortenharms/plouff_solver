program plouff_solver
	use mod_functions
	implicit none
	
	type(polygon), allocatable, dimension(:) :: poly
	type(control_struct) :: configurations 
	!integer :: j
	

	
	
	! read config-file at startup
	call read_config(configurations)
	
	call create_sphere(configurations,poly)
	
	call output_one_prism(configurations,poly)
	call output_sphere(configurations,poly)
	
	!do j=1,configurations%numberOfLayers
	!	print*, 'layer:',j,'depth:',poly(j)%z1
	!end do


end	program plouff_solver
