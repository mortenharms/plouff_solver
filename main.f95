program plouff_solver
	use mod_functions
	implicit none
	
	type(polygon), allocatable, dimension(:) :: poly
	type(control_struct) :: configurations 
    real, allocatable, dimension(:) :: profil_1d
	!integer :: j
	

	
	
	! read config-file at startup
	call read_config(configurations)
	
	call create_sphere(configurations,poly)
	
	!call output_one_prism(configurations,poly)
	!call output_sphere(configurations,poly)
	
    call calc_profil_vector(configurations)
    call profil_1d_x(configurations,poly,profil_1d)
        
    call save_profile_1d(configurations,profil_1d)
    
    
	!do j=1,configurations%numberOfLayers
	!	print*, 'layer:',j,'depth:',poly(j)%z1
	!end do


end	program plouff_solver
