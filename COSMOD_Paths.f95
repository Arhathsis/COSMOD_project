!=- fortran-libraries
!=- © Stanislav Shirokov, 2014-2020

module COSMOD_paths
   use global

      character(len) :: WorkDir='' , overleaf='' , MN_info

   contains

         subroutine paths

            WorkDir           =  trim( make_workdir() ) // 'Main_Workspace\'

				overleaf          = trim(WorkDir) // 'overleaf\'

            MN_info = trim(WorkDir)//'overleaf\MN.info'

            end subroutine

end module
