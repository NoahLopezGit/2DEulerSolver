program main
  use Cell_Geometry
  implicit none

  real, dimension(4,2) :: points
  real, dimension(2) :: centroid
  real :: area

  points(:,1) = (/0, 1, 1, 0/)
  points(:,2) = (/0, 0, 1, 1/)

  call calc_area(points, area)
  print *, area

  call calc_centroid(points, centroid)
  print *, centroid

end program main
