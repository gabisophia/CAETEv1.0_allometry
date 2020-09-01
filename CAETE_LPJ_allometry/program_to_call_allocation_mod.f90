program caete

	use allocation
	implicit none
	
	call leaf_carbon(delta_leafs)
	call root_carbon(delta_leafs, L, ltor, R, delta_root)
	call sapwood_carbon (delta_leafs, delta_root, bminc, delta_sapwood)

end program caete