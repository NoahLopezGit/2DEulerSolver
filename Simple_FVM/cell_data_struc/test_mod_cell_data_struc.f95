program test_mod_cell_data_struc
  use mod_cell_data_struc
  implicit none
  !this will test the cell data type
  !first try initializing
  type(Cell) :: myCell
  real, dimension(1) :: cell_quantity
  integer, dimension(5) :: neighbor_cell_ids, node_ids

  neighbor_cell_ids = (/1,2,3,4,5/)
  node_ids = (/4,6,7,8,9/)
  cell_quantity(1)=2.5

  myCell = init_cell(node_ids,neighbor_cell_ids,cell_quantity)
  print *, "Cell quantity ",myCell%cell_quantities
  print *, "Cell neighbor ids",myCell%neighbor_cell_ids
  print *, "Cell node ids",myCell%node_ids
end program test_mod_cell_data_struc
