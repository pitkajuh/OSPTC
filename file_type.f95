module file_type
  use section_type
  use interpolate
  implicit none

  type, abstract :: file
     integer :: n_ionization
   contains
     procedure(create_file), deferred :: create
     procedure(file_destructor), deferred :: file_clear
  end type file

  abstract interface
     subroutine create_file(this, z, ios, n)
       import file
       class(file), intent(inout) :: this
       integer, intent(inout) :: z, ios, n
     end subroutine create_file

     subroutine file_destructor(this)
       import file
       class(file), intent(inout):: this
       integer :: i
     end subroutine file_destructor
  end interface

  type, extends(file) :: MF23
     type(section_coherent_scattering) :: coherent_scattering
     type(section_incoherent_scattering) :: incoherent_scattering
     type(section_pair_formation) :: pair_formation_elec
     type(section_pair_formation) :: pair_formation_nuc
     type(section_photo_ionization), allocatable :: photo_ionization(:)
     real(kind(1.d0)), allocatable :: ionization_energies(:)
   contains
     procedure, pass :: create => create_mf23
     procedure, pass :: file_clear => clear_mf23
  end type MF23

  type, extends(file) :: MF27
     type(section_coherent_factor) :: coherent_factor
     type(section_incoherent_function) :: incoherent_function
     type(section_imaginary_factor) :: imaginary_factor
     type(section_real_factor) :: real_factor
   contains
     procedure, pass :: create => create_mf27
     procedure, pass :: file_clear => clear_mf27
  end type MF27

contains

  subroutine clear_mf27(this)
    class(MF27), intent(inout) :: this
    call section_destructor(this%coherent_factor)
    call section_destructor(this%incoherent_function)
    call section_destructor(this%imaginary_factor)
    call section_destructor(this%real_factor)
  end subroutine clear_mf27

  subroutine clear_mf23(this)
    class(MF23), intent(inout) :: this
    integer :: i
    call section_destructor(this%coherent_scattering)
    call section_destructor(this%incoherent_scattering)
    call section_destructor(this%pair_formation_elec)
    call section_destructor(this%pair_formation_nuc)

    do i=1, this%n_ionization
       call section_destructor(this%photo_ionization(i))
    end do

    deallocate(this%ionization_energies)
  end subroutine clear_mf23

  subroutine create_mf23(this, z, ios, n)
    class(MF23), intent(inout) :: this
    integer ::  MF, MT, i, n_photo, to, value_column_size
    integer, intent(inout) :: z, ios, n
    ! Skip 23501
    call this%coherent_scattering%skip_section(z, ios)
    call this%coherent_scattering%read_section_header(z, ios, MF, MT)
    value_column_size=int(this%coherent_scattering%header(1, 3))
    allocate(this%coherent_scattering%records(2, value_column_size))
    call this%coherent_scattering%read_section(z, ios, MF, MT, &
         value_column_size, this%coherent_scattering%records)

    call this%incoherent_scattering%read_section_header(z, ios, MF, MT)
    value_column_size=int(this%incoherent_scattering%header(1, 3))
    allocate(this%incoherent_scattering%records(2, value_column_size))
    call this%incoherent_scattering%read_section(z, ios, MF, MT, &
         value_column_size, this%incoherent_scattering%records)

    call this%pair_formation_elec%read_section_header(z, ios, MF, MT)
    value_column_size=int(this%pair_formation_elec%header(1, 3))-1
    this%pair_formation_elec%header(1, 3)=value_column_size
    allocate(this%pair_formation_elec%records(2, value_column_size))
    call this%pair_formation_elec%read_section(z, ios, MF, MT, &
         value_column_size, this%pair_formation_elec%records)

    ! Skip 23516
    call this%pair_formation_nuc%skip_section(z, ios)

    call this%pair_formation_nuc%read_section_header(z, ios, MF, MT)
    value_column_size=int(this%pair_formation_nuc%header(1, 3))-1
    this%pair_formation_nuc%header(1, 3)=value_column_size
    allocate(this%pair_formation_nuc%records(2, value_column_size))
    call this%pair_formation_nuc%read_section(z, ios, MF, MT, &
         value_column_size, this%pair_formation_nuc%records)

    ! Skip 23522
    n_photo=n-7
    allocate(this%photo_ionization(n_photo))
    call this%photo_ionization(1)%skip_section(z, ios)
    i=1

    do
       to=i
       call this%photo_ionization(to)%read_section_header(z, ios, MF, MT)
       if(MT==0 .and. MF==0) exit

       value_column_size=int(this%photo_ionization(to)%header(1, 3))-1
       this%photo_ionization(to)%header(1, 3)=value_column_size
       allocate(this%photo_ionization(to)%records(2, value_column_size))
       call this%photo_ionization(to)%read_section(z, ios, MF, MT, &
            value_column_size, this%photo_ionization(to)%records)
       i=i+1
    end do
    this%n_ionization=i-1
  end subroutine create_mf23

  subroutine create_mf27(this, z, ios, n)
    class(MF27), intent(inout) :: this
    integer, intent(inout) :: z, ios, n
    integer :: MF, MT, value_column_size, i

    call this%coherent_factor%read_section_header(z, ios, MF, MT)
    value_column_size=int(this%coherent_factor%header(1, 3))
    allocate(this%coherent_factor%records(2, value_column_size))
    call this%coherent_factor%read_section(z, ios, MF, MT, &
         value_column_size, this%coherent_factor%records)

    call this%incoherent_function%read_section_header(z, ios, MF, MT)
    value_column_size=int(this%incoherent_function%header(1, 3))-1
    this%incoherent_function%header(1, 3)=value_column_size
    allocate(this%incoherent_function%records(2, value_column_size))
    call this%incoherent_function%read_section(z, ios, MF, MT, &
         value_column_size, this%incoherent_function%records)

    call this%imaginary_factor%read_section_header(z, ios, MF, MT)
    value_column_size=int(this%imaginary_factor%header(1, 3))
    allocate(this%imaginary_factor%records(2, 2*value_column_size))
    call this%imaginary_factor%read_section(z, ios, MF, MT, &
         value_column_size, this%imaginary_factor%records)

    call this%real_factor%read_section_header(z, ios, MF, MT)
    value_column_size=int(this%real_factor%header(1, 3))
    allocate(this%real_factor%records(2, 2*value_column_size))
    call this%real_factor%read_section(z, ios, MF, MT, &
         value_column_size, this%real_factor%records)

  end subroutine create_mf27
end module file_type
