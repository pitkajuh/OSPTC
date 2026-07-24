program main
  use interpolate
  use random
  use cell_type
  use tape_type
  use radionuclide_type
  use material_type
  use physics_routine
  use photon_type
  implicit none
  integer, allocatable :: seed1(:)
  ! integer, dimension(100) :: ix
  integer :: n, time, count1, i
  integer :: second, cell_index
  class(material), allocatable :: steel1
  class(material), allocatable :: nitrogen1
  type(photon), pointer :: current_photon, ph, temp
  class(radionuclide), allocatable :: co_60_source
  logical :: cell_hit, continue_loop, reaction, end
  class(cells), allocatable :: cell_all(:)

  ! call random_seed(size=n)
  ! allocate(seed1(n))
  ! call system_clock(count=count1)
  ! print *, count1
  ! do i=1, n
  !    seed1(i)=count1+i*99999
  ! end do

  ! call random_seed(put=seed1)

  ! seed1=count
  ! call random_seed(put=ix(1:n))

  ! call random_seed(size=n)
  ! allocate(seed1(n))
  ! seed1=time()
  ! call random_seed(get=seed1)

  reaction=.false.
  cell_hit=.false.
  continue_loop=.false.
  allocate(steel :: steel1)
  call steel1%create()
  allocate(nitrogen :: nitrogen1)
  call nitrogen1%create()




  allocate(cell_all(2))

  allocate(cell_cylinder_truncated_z::cell_all(1)%cell_array)
  select type(cell_array => cell_all(1)%cell_array)
  class is (cell_cylinder_truncated_z)
     ! cell_array%name="source"
     cell_array%cell_material=steel1
     call cell_array%create(0.1_8, -0.1_8, 0.1_8, 0.0_8, 0.0_8, 0.0_8)
  end select

  allocate(cell_cylinder_truncated_z::cell_all(2)%cell_array)
  select type(cell_array => cell_all(2)%cell_array)
  class is (cell_cylinder_truncated_z)
     ! cell_array%name="outside"
     cell_array%cell_material=nitrogen1
     call cell_array%create(1.0_8, -1.0_8, 1.0_8, 0.0_8, 0.0_8, 0.0_8)
  end select

  ! print *, size(cell_all)

  ! new_location=coordinate(0.1_8, 0.1_8, 0.1_8)
  ! print *, source_cell%cell_test(new_location)







  ! ph%energy=1
  ! current_photon=>ph

  ! do i=2, 10
  !    allocate(current_photon%next_photon)
  !    current_photon=>current_photon%next_photon
  !    current_photon%energy=i
  !    current_photon%next_photon => null()
  ! end do

  ! ! previous_photon=>null()
  ! current_photon=>ph

  ! do while (associated(current_photon))
  !    ! print *, current_photon%energy
  !    ! previous_photon=>current_photon
  !    current_photon=>current_photon%next_photon
  ! end do



  allocate(co_60 :: co_60_source)
  co_60_source%activity=10!E4

  allocate(ph)
  end=.false.
  do second=1, 1!00

     ! do i=1, co_60_source%activity
        cell_index=1
        ph=co_60_source%pdf()
        ph%id=1
        ph%origin=cell_all(cell_index)%cell_array%random_initial_position()

        ph%next_photon=>null()
        current_photon=>ph
        print *, "----- origin begin"
        call show(ph%origin)
        print *, "-----"
        do while (associated(current_photon))
           end=surface_tracking(cell_all, current_photon, cell_index)

           if(end .eqv. .false.) then
              cycle
           end if

           current_photon=>current_photon%next_photon
        end do
     ! end do
  end do
  print *, "END"
  deallocate(co_60_source)

  current_photon=>ph

  do while(associated(current_photon))
     temp=>current_photon
     current_photon=>current_photon%next_photon
     if(associated(temp)) then
        deallocate(temp)
     end if
  end do

  ! if(associated(ph)) then
  !     deallocate(ph)
  ! end if



  ! do i=1, size(cell_all)
  !    ! deallocate(cell_all(i))
  !    ! select type(cell_array => cell_all(i)%cell_array)

  !    !    print *,cell_all(i)%name
  !    !    ! cell_all%cell_array(i)!%surface_cylinder%v
  !    ! end select
  !    ! print *, cell_all(i)%cell_array%name
  !    ! deallocate(cell_all(i)%cell_array%cell_material%mu)
  !    ! deallocate(cell_all(i)%cell_array%cell_material%endf%coherent_A)
  !    ! deallocate(cell_all(i)%cell_array%cell_material%endf%mf23%photo_ionization)
  !    ! call clear_mf23(cell_all(i)%cell_array%cell_material%endf%mf23)
  !    ! call clear_mf27(cell_all(i)%cell_array%cell_material%endf%mf27)
  !    ! ! print *, cell_all(i)%cell_array%cell_test(new_location)
  ! end do


  deallocate(steel1)
  deallocate(nitrogen1)

  deallocate(cell_all)

end program main
