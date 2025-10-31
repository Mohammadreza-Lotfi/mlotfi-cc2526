program verlet3D
implicit none
integer :: k
real :: x, y, z, vx, vy, vz, fx, fy, fz, t, tau, m

tau = 0.2
m = 1.0

x = 0.0
y = 0.0
z = 0.0
vx = 0.0
vy = 0.0
vz = 0.0

fx = 0.0
fy = 0.1
fz = 0.0

print *, "Time X Y Z"

do k = 1,10
    t = k*tau
    print *, t, x, y, z
    x = x + tau*vx + 0.5*fx*tau*tau/m
    y = y + tau*vy + 0.5*fy*tau*tau/m
    z = z + tau*vz + 0.5*fz*tau*tau/m
    vx = vx + tau*fx/m
    vy = vy + tau*fy/m
    vz = vz + tau*fz/m
end do

end program verlet3D

