module CellType
  type Cell
     real, dimension(4,2) :: nodes
     real, dimension(4)   :: states
  end type Cell
end module CellType
