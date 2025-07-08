module surface_type
  implicit none

  type :: surface
     ! For general quadratic surface S(x, y, z)=Ax^2+By^2+Cz^2+Dxy+Eyz+F zx+Gx+Hy+Iz+J
  real(kind(1.d0)) value, A, B, C, D, E, F, G, H, I, J, L, M, K

  ! A=0
  ! B=0
  ! C=0
  ! D=0
  ! E=0
  ! F=0
  ! G=0
  ! H=0
  ! I=0
  ! J=0
  ! L=0
  ! M=0
  ! K=0
  end type surface





end module surface_type
