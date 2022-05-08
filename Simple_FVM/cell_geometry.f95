module Cell_Geometry
  implicit none
  !define variables up here

  !points in determined at runtime
  !Think I want to use subroutines for their increased functionality (return multiple values)
contains
  !points will be a nx2 array with n points defined with and x and y coordinate value
  !the points will be arranged counterclockwise around the polygon
  subroutine calc_area(points, area)
    implicit none !this has to be declared in a subroutine... almost like this is a separate program
    !arrays with unknown size use assumed shape arguements
    !this must be used w/in module (which generates implicit interface) or have an explicit interface
    real, intent(in)  :: points(:,:)
    real, intent(out) :: area

    !local variables
    integer :: num_points, num_coords
    integer :: i  !iterables

    num_points = size(points,1)
    num_coords = size(points,2) !we know this should be 2 for x and y

    area = 0.0
    do i=1,num_points-1
      area = area + points(i,1)*points(i+1,2) - points(i+1,1)*points(i,2)
    end do
    area = 0.5 * area
  end subroutine calc_area

  subroutine calc_centroid(points, centroid)
    implicit none

    real, intent(in) :: points(:,:)
    real, intent(out), dimension(2) :: centroid !x and y coordinate in array

    !local variables
    integer :: num_points, num_coords
    integer :: i
    real    :: area
    real :: Cx, Cy
    real :: x1, x2, y1, y2

    num_points = size(points,1)

    !must first calculate the area
    area = 0.0
    do i=1,num_points-1
      print *, i
      area = area + points(i,1)*points(i+1,2) - points(i+1,1)*points(i,2)
    end do
    area = 0.5 * area

    Cx = 0.0
    Cy = 0.0
    do i=1,num_points-1
      print *, i
      x1 = points(i,1)
      x2 = points(i+1,1)
      y1 = points(i,2)
      y2 = points(i+1,2)

      Cx = Cx + (x1+x2)*(x1*y2-x2*y1)
      Cy = Cy + (y1+y2)*(x1*y2-x2*y1)
    end do
    Cx = 1/(6*area)*Cx
    Cy = 1/(6*area)*Cy
    centroid(:) = (/Cx,Cy/)
  end subroutine calc_centroid

end module Cell_Geometry
