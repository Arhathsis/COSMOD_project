!=- fortran-libraries
!=- © Stanislav Shirokov, 2014-2020

   module COSMOD_citadel
         use global

         use COSMOD_paths
         use COSMOD_graphics
         use COSMOD_scripts

      contains



         subroutine caption_MN_info

            open(unit_1,file=MN_info,status='replace')
               write(unit_1,*) 'Anyone with this link can edit this project'
               write(unit_1,*) 'https://www.overleaf.com/9181497425tkqhjdzzmbgg'
               write(unit_1,*) 'Anyone with this link can view this project'
               write(unit_1,*) 'https://www.overleaf.com/read/jjrmkfbhhftv'
               close(unit_1)

            end subroutine caption_MN_info



         subroutine preparation

            call paths

            write(*,*) 'COSMOD citadel is runned..'

            end subroutine



         subroutine commands

            call preparation

            do ; write(*,*) 'Enter the command:' ; read (*,*) command

               select case (command)

                  case ('overleaf') ; call make_overleaf_figures_folder
                     case ('o') ; call make_overleaf_figures_folder

                  case ('exit') ; exit

                  case default ; write(*,*) 'unknown command'

                  end select

               enddo

            end subroutine



         subroutine make_overleaf_figures_folder

            call system('echo off && MD ' // trim(overleaf) // ' >> log.log ' )

            call caption_MN_info

            end subroutine



      end module
