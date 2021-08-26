!=- fortran-libraries
!=- © Stanislav Shirokov, 2014-2020

module COSMOD_paths
   use global

      character(len) :: WorkDir , Models_folder , Tables_folder , overleaf_dir , &

                        paper_name = 'NaN'

   contains

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

         subroutine paths

            !=- COSMOD folders
            WorkDir           =  trim( make_workdir() ) // 'Main_Workspace\'

            Models_folder     =  trim( WorkDir        ) // 'Models\'
            Tables_folder     =  trim( WorkDir        ) // 'Tables\'

            overleaf_dir      =  trim( WorkDir        ) // 'overleaf\'

            !=- creating folders from the Folders array
               folders(1) = WorkDir
               folders(2) = Models_folder
               folders(3) = Tables_folder
               folders(4) = overleaf_dir
            call shell_MD_Tree

            end subroutine

         !=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=%=!

end module
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- truncated=136-=1
