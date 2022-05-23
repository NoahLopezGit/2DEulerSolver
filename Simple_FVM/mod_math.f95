module mod_math
  implicit none

contains
  function cross(vector1,vector2)result(vector_cross)
    implicit none
    real, dimension(3) :: vector1, vector2, vector_cross
    !in this form: vector_cross=vector1 x vector2
    vector_cross(1)=vector1(2)*vector2(3)-vector1(3)*vector2(2)
    vector_cross(2)=vector1(3)*vector2(1)-vector1(1)*vector2(3)
    vector_cross(3)=vector1(1)*vector2(2)-vector1(2)*vector2(1)
  end function cross
end module mod_math
