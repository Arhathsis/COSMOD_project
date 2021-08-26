!=- fortran-libraries
!=- © Stanislav Shirokov, 2014-2020

   module COSMOD_citadel
         use global

         use COSMOD_paths
         use COSMOD_graphics
         use COSMOD_tables
         use COSMOD_scripts
         use COSMOD_overleaf

      contains

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         subroutine commands
            call preparation
				do
               call input_command(commandN)
					select case (commandN(1))

               !=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=!

                  case('mn2021a')
                     call mn2021a

                  case('mn2020b')
                     call mn2020b

                  case('mn2020a')
                     call mn2020a

                  case('example')
                     call example

               !=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=!

                  case('exit')
                     exit

                  case default
                     write(*,*) 'unknown command'

                  end select

                  call set_default

               enddo
            end subroutine

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         subroutine mn2021a
            !=- If there is more one argument in command       -=1
            if ( word_search_array( commandN , 'fig1' ) ) call mn2021a_fig1
            !if ( word_search_array( commandN , 'tab1' ) ) call mn2021a_plot_tab1
            !if ( word_search_array( commandN , 'o'    ) ) call mn2021a_overleaf
            !=- default command without additional arguments   -=1
            if ( commandN(2)=='NaN' ) then
               call mn2021a_fig1
               !call mn2021a_plot_tab1
               !call mn2021a_overleaf
               end if

            end subroutine

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         subroutine mn2020b
            !=- If there is more one argument in command       -=1
            if ( word_search_array( commandN , 'fig1' ) ) call mn2020b_plot_fig1
            if ( word_search_array( commandN , 'tab1' ) ) call mn2020b_plot_tab1
            if ( word_search_array( commandN , 'o'    ) ) call mn2020b_overleaf
            !=- default command without additional arguments   -=1
            if ( commandN(2)=='NaN' ) then
               call mn2020b_plot_tab1
               call mn2020b_plot_fig1
               call mn2020b_overleaf
               end if

            end subroutine

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         subroutine mn2020a
            !=- If there is more one argument in command       -=1
            if ( word_search_array( commandN , 'fig3' ) ) call mn2020a_plot_fig3
            if ( word_search_array( commandN , 'o'    ) ) call mn2020a_overleaf
            !=- default command without additional arguments   -=1
            if ( commandN(2)=='NaN' ) then
               call mn2020a_plot_fig3
               call mn2020a_overleaf
               end if

            end subroutine

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         subroutine preparation
            call define_system
            call paths
               write(*,*) 'COSMOD citadel has runned'
            end subroutine

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         subroutine input_command(commandN)
            character(len) command,commandN(:)
               33 continue ; write(*,*) '--' ; commandN(:)='NaN'

					read(*,'(A256)',err=33,end=44) command
                  read(command,*,iostat=iostat_value,err=33,end=44) commandN ; 44 continue

            end subroutine

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         subroutine set_default  !=- ?

            call MS_default
            call overleaf_default
            call math_default

            end subroutine

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

      end module
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- truncated=136-=1
