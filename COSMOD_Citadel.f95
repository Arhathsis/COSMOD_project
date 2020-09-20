!=- fortran-libraries
!=- © Stanislav Shirokov, 2014-2020

   module COSMOD_citadel
         use global

         use COSMOD_paths
         use COSMOD_graphics
         use COSMOD_tables
         use COSMOD_scripts

      character(len) :: OS

      contains





         subroutine preparation

            call define_system
               if (operation_system==1) OS = 'Windows'
                  write(*,*) 'OS: ',trim(OS)
            call paths
            call gen_folders

            write(*,*) 'COSMOD citadel is run..'

            end subroutine



         subroutine commands

            call preparation

            do ; write(*,*) 'Enter the command:' ; read (*,*) command

               select case (command)

                  case ('overleaf') ; call make_overleaf_figures_folder
                     case ('o') ; call make_overleaf_figures_folder

                  case ('cosmod')
                     call COSMOD_model_set
                     case ('c')
                        call COSMOD_model_set

                  case ('fig1') ; call MN_Letter_plot_fig1

                  case ('tab1') ; call MN_Letter_plot_tab1

                  case ('exit') ; exit

                  case default ; write(*,*) 'unknown command'

                  end select

               enddo

            end subroutine




      end module
