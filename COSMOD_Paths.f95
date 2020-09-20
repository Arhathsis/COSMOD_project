!=- fortran-libraries
!=- © Stanislav Shirokov, 2014-2020

module COSMOD_paths
   use global

      character(len) :: WorkDir='' , overleaf='' , MN_info , Models_folder , path_tabs

   contains

         subroutine paths

            WorkDir           =  trim( make_workdir() ) // 'Main_Workspace\'

            Models_folder     =  trim(WorkDir) // 'Models\'

				overleaf          =  trim(WorkDir)  // 'overleaf\MN_Letter\'
            MN_info           =  trim(overleaf) // 'MN.info'

            path_tabs         =  trim(WorkDir) // 'Tables\'

            end subroutine



         subroutine gen_folders

            call system('mkdir ' // trim(Models_folder) // ' >> log.log ' )
            call system('mkdir ' // trim(path_tabs) // ' >> log.log ' )

            call sleep (1)

            call system('cls')

            end subroutine gen_folders



         subroutine caption_MN_info

            open(unit_1,file=MN_info,status='replace')
               write(unit_1,*) 'Anyone with this link can edit this project'
               write(unit_1,*) 'https://www.overleaf.com/5957249232dfnmwjsjvdng'
               write(unit_1,*) 'Anyone with this link can view this project'
               write(unit_1,*) 'https://www.overleaf.com/read/ybwtxspzsdtb'
               close(unit_1)

            end subroutine caption_MN_info


         subroutine make_overleaf_figures_folder

            call system('echo off && MD ' // trim(overleaf) // ' >> log.log ' )

            files( 1  ) = trim(WorkDir) // &
               'plots\MN_letter_model_set\MN_letter_model_set_mu(z)_all_models.eps'
               new_files( 1  ) = 'Shirokov_fig1.eps'

            files( 2  ) = trim(WorkDir) // &
               'plots\MN_letter_model_set\MN_letter_model_set_delta_mu(z)_all_models.eps'
               new_files( 2  ) = 'Shirokov_fig2.eps'

            files( 3  ) = trim(WorkDir) // &
               'plots\MN_letter_model_set\MN_letter_model_set_delta_mu(z)_fig1a.eps'
               new_files( 3  ) = 'Shirokov_fig1a.eps'

            files( 4  ) = trim(WorkDir) // &
               'plots\MN_letter_model_set\MN_letter_model_set_delta_mu(z)_fig1b.eps'
               new_files( 4  ) = 'Shirokov_fig1b.eps'

            do i=1,N_files
               if (files(i)/='') call system('copy '// trim(files(i)) //' '// trim(overleaf) // ' >> log.log ' )
               if (files(i)/='') call system('move '// trim(overleaf) // trim(name(files(i))) //'.eps '// &
                  trim(overleaf) // trim(new_files(i)) // ' >> log.log ' )
               enddo

            call caption_MN_info

            end subroutine


end module
