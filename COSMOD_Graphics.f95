!=- fortran-libraries
!=- © Stanislav Shirokov, 2014-2020

   module COSMOD_graphics
      use global
      use GNUplot
      use cosmology
      use math

      use COSMOD_config
      use COSMOD_paths

      character(len) caption

      integer :: Omega_v_count , Omega_k_count , w_count

      contains




         subroutine MN_Letter_fig1
            integer i,j,k,ii,jj, col , start_position , i_max , k_max , j_max , n_LCDM , n_model, GNUfields_plots_i_start
            character(len) GRB_catalog_path , GRB_medians_path

               GRB_catalog_path = 'C:\Users\Arhath\YandexDisk\Science\DATA\GRB_catalogs\GRBdata_Amati-193.dat'
               GRB_medians_path = 'C:\Users\Arhath\YandexDisk\Science\DATA\GRB_catalogs\GRB_log_medians_data.dat'

               call clear_plot_fields

               pngterm  = 'set term pngcairo font "Verdana,10" size 1920, 1080'  !=-  enhanced
               epsterm  = 'set term postscript enhanced color font "Verdana,16" size 8.5, 8.5'

               GNUfields(32) = 'set termoption dashed'

               start_position = 33
                      plot1 = 100
                     title1 = 101

               GNUplot_colors(1) = 'black'
               GNUplot_colors(2) = 'red'
               GNUplot_colors(3) = 'blue'

               i_max = w_count
               k_max = Omega_k_count
               j_max = Omega_v_count

               if ( MS_real_model_count /= i_max*j_max*k_max ) write(*,*) &
                  'COSMOD_Graphics: MN_Letter_fig1: incorrect model count'

               n_LCDM = 8

               n_model = 0
               do j=1,j_max         !=- Omega_v   1-2*(j-1)*(j-3)
                  do i=1,i_max      !=- w
                     do k=1,k_max   !=- Omega_k & color
                           if (j==1) thickness = 2
                           if (j==2) thickness = 4
                           if (i==1) dash_type = 6
                           if (i==2) dash_type = 1
                           !if (k==3) thickness = 2
                           !if (i==1 .and. j==2) dash_type = 6
                           !if (i==2 .and. j==2) dash_type = 1
                           !if (i==1 .and. j==1) dash_type = 3
                           !if (i==2 .and. j==1) dash_type = 4
                        n_model = n_model + 1
                        GNUfields( start_position+n_model ) = 'set linestyle ' // &
                           trim(inttostr( n_model )) // &
                           ' lw ' //trim(inttostr( thickness )) // ' pt 7 ps 0.7 dt ' // &
                           trim(inttostr( dash_type )) // &
                           ' lc rgb "' // trim( GNUplot_colors(k) ) // '"'
                        enddo
                     enddo
                  enddo

               if (GRB_medians_flag) then
                  GNUfields( start_position + n_model + 1 ) = 'set linestyle ' // &
                     trim(inttostr( n_model + 1 )) // &
                     ' lw 2 pt 7 ps 0.9 dt 1 lc rgb "dark-green"'
                  GNUfields( plot1+MS_real_model_count*2  ) = '"' // trim(slashfix(GRB_medians_path)) // &
                     '" u 1:5:3 w yerrorb ls ' // trim(inttostr( n_model + 1 ))
                  GNUfields( title1+MS_real_model_count*2  ) = ' title "  GRB_{medians}(Amati-193)"'
               endif

               GNUfields(logscale)  =  'set logscale x'
               GNUfields(xrange)    =  '[0.1:10]'

               GNUfields(legend)    =  'top left'
               GNUfields(xlabel)    =  'redshift z'
               GNUfields(grid)      =  '#xtics ytics mxtics mytics'
               GNUfields(mxtics)    =  '10'
               GNUfields(mytics)    =  '5'

               MS_fields(1) = '{/Symbol m}(z)'

               MS_fields(2) = '{/Symbol D}{/Symbol m}(z)'
               !MS_fields(3) = '{/Symbol m}(z) - {/Symbol m}_{model-' // trim(inttostr(n_LCDM)) // '}(z)'
               MS_fields(3) = '{/Symbol m}(z) - {/Symbol m}_{{/Symbol L}CDM}(z)'
               MS_fields(4) = 'mu(z)_'
               MS_fields(5) = 'delta_mu(z)_'
               MS_fields(6) = '{/Symbol D}{/Symbol m}(z)_'

               !MS_fields(2) = 'log_{10} ( 1 + {/Symbol D}{/Symbol m}(z) )'                     !=- log10
               !MS_fields(3) = 'log_{10} ( 1 + {/Symbol m}(z) - {/Symbol m}_{model-1}(z) )'   !=- log10
               !MS_fields(6) = 'log_{10} 1+{/Symbol D}{/Symbol m}(z)_'                           !=- log10

               do k=2,2    !=- k=1 - absolutes, k=2 - residuals
                  do j=3,3

                     GNUfields(title)  = trim(MS_fields(1+5*(k-1))) //' for different models'

                     do i=1,MS_real_model_count
                        col = 1+j+3*(i-1)

                        select case (k)
                           case (1)
                              GNUfields(ylabel) = trim(MS_fields(1))
                              GNUfields(yrange) =  '[38:*]'

                              GNUfields (plot1+(i-1)*2) = ', "' // trim(slashfix(model_set_path)) // &
                                 '" u 1:' // trim( inttostr( col ) ) // ' w l ls ' // trim(inttostr(i))
                           case(2)
                              GNUfields(yrange) =  '[*:*]'
                              GNUfields(yrange) =  '[-1:1.6]'
                              if (GRB_medians_flag) GNUfields(yrange) =  '[-2.3:1.6]'

                              GNUfields(ylabel) = trim(MS_fields(2)) // ' = ' // &
                                 trim(MS_fields(3))

                              GNUfields (plot1+(i-1)*2) = '"' // trim(slashfix(model_set_path)) // &
                                 '" u 1:(($' // trim( inttostr( col ) ) // ' - $' // &
                                 !'" u 1:(log10(10+$' // trim( inttostr( col ) ) // ' - $' // &   !=- log10
                                 trim(inttostr( 1+j+3*(n_LCDM-1) )) // ')) w l ls ' // trim(inttostr(i))  !=- log10
                           end select

                        !=- GNUfields (title1+(i-1)*2) = 'model-' // trim(adjustl(inttostr(i))) // ':' // &   !=-
                        GNUfields (title1+(i-1)*2) =  &   !=-
                           trim( model_set (i,2) )
                        if (GRB_medians_flag) GNUfields(title1+(i-1)*2) = 'notitle'
                        end do


                        if (GRB_medians_flag) then
                           caption='fig1b'
                           else
                              caption='fig1a'
                           endif
                     graph_name = trim(MS_fields(4+1*(k-1))) // trim(caption)
                        GNUfields(extention_out_figure)='#eps' ; call plot(model_set_path)
                     graph_name = trim(MS_fields(4+1*(k-1))) // trim(caption)
                        GNUfields(extention_out_figure)='#png' ; call plot(model_set_path)
                     enddo
                  enddo

            end subroutine MN_Letter_fig1




         subroutine MN_Letter
            integer i,j,k,ii,jj, col
               call clear_plot_fields

               GNUfields(logscale)  =  '#set logscale'
               GNUfields(format_y)  =  '#set format y "10^{%L}"'
               GNUfields(xrange)    =  'set xrange [0.1:*]'

               GNUfields(legend)    =  'set key right bottom'
               GNUfields(xlabel)    =  'set xlabel "redshift z"'
               GNUfields(grid)      =  'set grid xtics ytics mxtics mytics'
               GNUfields(mxtics)    =  'set mxtics 4'
               GNUfields(mytics)    =  'set mytics 5'

               MS_fields(1) = '{/Symbol m}(z)'
               !MS_fields(2) = 'log10 {/Symbol D}{/Symbol m}(z)'
               !MS_fields(3) = 'log10 ( {/Symbol m}(z) - {/Symbol m}_{model-1}(z) )'
               MS_fields(2) = '{/Symbol D}{/Symbol m}(z)'
               MS_fields(3) = '{/Symbol m}(z) - {/Symbol m}_{model-1}(z)'
               MS_fields(4) = 'mu(z)_'
               MS_fields(5) = 'delta_mu(z)_'
               MS_fields(6) = '{/Symbol D}{/Symbol m}(z)_'
         do jj=1,3
            do ii=1,3
               MS_fields(7) = 'w_-1'
               MS_fields(8) = 'w_-2'
               MS_fields(9) = 'w_-3'
               do k=1,2
                  do j=3,3

                     GNUfields(title)  = 'set title "'// trim(MS_fields(1+5*(k-1))) //' for different models"'

                     do i=1,3
                        col = 1+j+3*(i-1)+9*(ii-1)+27*(jj-1)
                        select case (k)
                           case (1)
                              GNUfields(ylabel) = 'set ylabel "' // trim(MS_fields(1)) // '"'
                              GNUfields(yrange) =  'set yrange [38:58]'

                              GNUfields (plot1+(i-1)*2) = ', "' // trim(slashfix(model_set_path)) // &
                                 '" u 1:' // trim( inttostr( col ) ) // ' w l ls ' // trim(inttostr(i))
                              GNUfields (title1+(i-1)*2) = ' title "model-' // trim(adjustl(inttostr(i))) // ':' // &
                                 trim( model_set (i+3*(ii-1) + 9*(jj-1),2) ) // '"'
                           case(2)
                              GNUfields(yrange) =  'set yrange [0.97:1.08]'
                              !GNUfields(yrange) =  'set yrange [-0.05:-0.05]'

                              GNUfields(ylabel) = 'set ylabel "' // trim(MS_fields(2)) // ' = ' // &
                                 trim(MS_fields(3)) // '"'

                              GNUfields (plot1+(i-1)*2) = ', "' // trim(slashfix(model_set_path)) // &
                                 '" u 1:(($' // trim( inttostr( col ) ) // '/$' // &   !=- log10
                                 trim(inttostr( col - 3*(i-1) )) // ')) w l ls ' // trim(inttostr(i))
                              GNUfields (title1+(i-1)*2) = ' title "model-' // trim(adjustl(inttostr(i))) // ':' // &
                                 trim( model_set (i+3*(ii-1) + 9*(jj-1),2) ) // '"'
                           end select
                        end do
                     GNUfields (plot1) = GNUfields (plot1)(2:len)

                     !graph_name = trim(MS_fields(4+1*(k-1))) // 'Ov_' // trim(MS_parameters(jj)) // '_' //  &
                     !   trim(MS_fields(6+ii))
                     !   GNUfields(extention_out_figure)='#eps' ; call plot(model_set_path)
                     graph_name = trim(MS_fields(4+1*(k-1))) // 'Ov_' // trim(MS_parameters(jj)) // '_' //  &
                        trim(MS_fields(6+ii))
                        GNUfields(extention_out_figure)='#png' ; call plot(model_set_path)
                     enddo
                  enddo
               enddo
            enddo
            end subroutine MN_Letter



         subroutine MN_Letter_all
            integer i,j,k,ii,jj, col
               call clear_plot_fields

               pngterm  = 'set term pngcairo font "Verdana,10" size 1920, 1080'  !=-  enhanced
               epsterm  = 'set term postscript enhanced color font "Verdana,14" size 19.2, 10.8'

               do i=1,3
                  GNUfields( 33+1+9*(i-1) ) = 'set linestyle ' // trim(inttostr( 1+9*(i-1)) ) // &
                     ' lw 1 pt 7 ps 0.7 dt ' // trim(inttostr(4+i)) // ' lc rgb "red"'
                  GNUfields(33+2+9*(i-1)) = 'set linestyle ' // trim(inttostr(2+9*(i-1))) // &
                     ' lw 1 pt 7 ps 0.7 dt ' // trim(inttostr(4+i)) // ' lc rgb "blue"'
                  GNUfields(33+3+9*(i-1)) = 'set linestyle ' // trim(inttostr(3+9*(i-1))) // &
                     ' lw 1 pt 7 ps 0.7 dt ' // trim(inttostr(4+i)) // ' lc rgb "dark-green"'
                  GNUfields(33+4+9*(i-1)) = 'set linestyle ' // trim(inttostr(4+9*(i-1))) // &
                     ' lw 1 pt 7 ps 0.7 dt ' // trim(inttostr(4+i)) // ' lc rgb "yellow"'
                  GNUfields(33+5+9*(i-1)) = 'set linestyle ' // trim(inttostr(5+9*(i-1))) // &
                     ' lw 1 pt 7 ps 0.7 dt ' // trim(inttostr(4+i)) // ' lc rgb "orange"'
                  GNUfields(33+6+9*(i-1)) = 'set linestyle ' // trim(inttostr(6+9*(i-1))) // &
                     ' lw 1 pt 7 ps 0.7 dt ' // trim(inttostr(4+i)) // ' lc rgb "black"'
                  GNUfields(33+7+9*(i-1)) = 'set linestyle ' // trim(inttostr(7+9*(i-1))) // &
                     ' lw 1 pt 7 ps 0.7 dt ' // trim(inttostr(4+i)) // ' lc rgb "purple"'
                  GNUfields(33+8+9*(i-1)) = 'set linestyle ' // trim(inttostr(8+9*(i-1))) // &
                     ' lw 1 pt 7 ps 0.7 dt ' // trim(inttostr(4+i)) // ' lc rgb "brown"'
                  GNUfields(33+9+9*(i-1)) = 'set linestyle ' // trim(inttostr(9+9*(i-1))) // &
                     ' lw 1 pt 7 ps 0.7 dt ' // trim(inttostr(4+i)) // ' lc rgb "dark-cyan"'
               end do
               GNUfields(32) = 'set termoption dashed'

               GNUfields(logscale)  =  'set logscale x'
               GNUfields(format_y)  =  '#set format y "10^{%L}"'
               GNUfields(xrange)    =  'set xrange [0.1:*]'

               GNUfields(legend)    =  'set key top left'
               GNUfields(xlabel)    =  'set xlabel "redshift z"'
               GNUfields(grid)      =  'set grid xtics ytics mxtics mytics'
               GNUfields(mxtics)    =  'set mxtics 10'
               GNUfields(mytics)    =  'set mytics 5'

               MS_fields(1) = '{/Symbol m}(z)'
               !MS_fields(2) = 'log10 {/Symbol D}{/Symbol m}(z)'
               !MS_fields(3) = 'log10 ( {/Symbol m}(z) - {/Symbol m}_{model-1}(z) )'
               MS_fields(2) = '{/Symbol D}{/Symbol m}(z)'
               MS_fields(3) = '{/Symbol m}(z) - {/Symbol m}_{model-1}(z)'
               MS_fields(4) = 'mu(z)_'
               MS_fields(5) = 'delta_mu(z)_'
               MS_fields(6) = '{/Symbol D}{/Symbol m}(z)_'

               do k=1,2
                  do j=3,3

                     GNUfields(title)  = 'set title "'// trim(MS_fields(1+5*(k-1))) //' for different models"'
                      plot1 = 100
                     title1 = 101

                     do i=1,27
                        col = 1+j+3*(i-1)
                        select case (k)
                           case (1)
                              GNUfields(ylabel) = 'set ylabel "' // trim(MS_fields(1)) // '"'
                              GNUfields(yrange) =  'set yrange [*:*]'

                              GNUfields (plot1+(i-1)*2) = ', "' // trim(slashfix(model_set_path)) // &
                                 '" u 1:' // trim( inttostr( col ) ) // ' w l ls ' // trim(inttostr(i))
                              GNUfields (title1+(i-1)*2) = ' title "model-' // trim(adjustl(inttostr(i))) // ':' // &
                                 trim( model_set ( i,2 )) // '"'
                           case(2)
                              GNUfields(yrange) =  'set yrange [*:*]'
                              !GNUfields(yrange) =  'set yrange [-0.05:-0.05]'

                              GNUfields(ylabel) = 'set ylabel "' // trim(MS_fields(2)) // ' = ' // &
                                 trim(MS_fields(3)) // '"'

                              GNUfields (plot1+(i-1)*2) = ', "' // trim(slashfix(model_set_path)) // &
                                 '" u 1:(($' // trim( inttostr( col ) ) // ' - $' // &   !=- log10
                                 trim(inttostr( 1+j )) // ')) w l ls ' // trim(inttostr(i))
                              GNUfields (title1+(i-1)*2) = ' title "model-' // trim(adjustl(inttostr(i))) // ':' // &
                                 trim( model_set (i,2) ) // '"'
                           end select
                        end do
                     GNUfields (plot1) = GNUfields (plot1)(2:len)

                     graph_name = trim(MS_fields(4+1*(k-1))) // 'all_models'
                        GNUfields(extention_out_figure)='#eps' ; call plot(model_set_path)
                     graph_name = trim(MS_fields(4+1*(k-1))) // 'all_models'
                        GNUfields(extention_out_figure)='#png' ; call plot(model_set_path)
                     enddo
                  enddo

            end subroutine MN_Letter_all



         subroutine MN_Letter_example
            integer i,j,k,ii,jj, col , start_position , i_max , k_max , j_max , n_LCDM , n_model
            character(len) GRB_catalog_path , GRB_medians_path

               GRB_catalog_path = 'C:\Users\Arhath\YandexDisk\Science\DATA\GRB_catalogs\GRBdata_Amati-193.dat'
               GRB_medians_path = 'C:\Users\Arhath\YandexDisk\Science\DATA\GRB_catalogs\GRB_log_medians_data.dat'

               call clear_plot_fields

               pngterm  = 'set term pngcairo font "Verdana,10" size 1920, 1080'  !=-  enhanced
               epsterm  = 'set term postscript enhanced color font "Verdana,16" size 8.5, 8.5'

               GNUfields(32) = 'set termoption dashed'

               GNUplot_colors(1) = 'black'
               GNUplot_colors(2) = 'red'
               GNUplot_colors(3) = 'blue'

               start_position = 33
               i_max = w_count
               k_max = Omega_k_count
               j_max = Omega_v_count

               if ( MS_real_model_count /= i_max*j_max*k_max - 6 ) write(*,*) &
                  'COSMOD_Graphics: MN_Letter_example: incorrect model count'

               n_LCDM = 5

               n_model = 0
               do j=1,j_max         !=- Omega_v   1-2*(j-1)*(j-3)
                  if ( j /= 2 ) then
                     i_max = 1
                     else
                        i_max = w_count
                     endif
                  do i=1,i_max      !=- w
                     do k=1,k_max   !=- Omega_k & color
                        if ( j==2 .and. i==1 ) then
                           thickness = 4
                           else
                              thickness = 2
                           endif
                        n_model = n_model + 1
                        GNUfields( start_position+n_model ) = 'set linestyle ' // &
                           trim(inttostr( n_model )) // &
                           ' lw ' //trim(inttostr( thickness )) // ' pt 7 ps 0.7 dt ' // &
                           trim(inttostr( 2*(j-2)*(j-3)-(j-1)*(j-3)+6*(j-1)*(j-2)/2 )) // &
                           ' lc rgb "' // trim( GNUplot_colors(k) ) // '"'
                        enddo
                     enddo
                  enddo
               GNUfields(start_position+n_model+1) = 'set linestyle ' // &
                  trim(inttostr( MS_real_model_count+1 )) // 'pt 7 ps 0.9 lc rgb "white"'
               GNUfields(start_position+n_model+2) = 'set linestyle ' // &
                  trim(inttostr( MS_real_model_count+2 )) // 'pt 7 ps 0.9 lc rgb "dark-green"'

               GNUfields(logscale)  =  'set logscale x'
               GNUfields(format_y)  =  '#set format y "10^{%L}"'
               GNUfields(xrange)    =  'set xrange [0.1:10]'

               GNUfields(legend)    =  'set key top left'
               GNUfields(xlabel)    =  'set xlabel "redshift z"'
               GNUfields(grid)      =  'set grid xtics ytics mxtics mytics'
               GNUfields(mxtics)    =  'set mxtics 10'
               GNUfields(mytics)    =  'set mytics 5'

               MS_fields(1) = '{/Symbol m}(z)'

               MS_fields(2) = '{/Symbol D}{/Symbol m}(z)'
               MS_fields(3) = '{/Symbol m}(z) - {/Symbol m}_{model-' // trim(inttostr(n_LCDM)) // '}(z)'
               MS_fields(4) = 'mu(z)_'
               MS_fields(5) = 'delta_mu(z)_'
               MS_fields(6) = '{/Symbol D}{/Symbol m}(z)_'

               !MS_fields(2) = 'log_{10} ( 1 + {/Symbol D}{/Symbol m}(z) )'                     !=- log10
               !MS_fields(3) = 'log_{10} ( 1 + {/Symbol m}(z) - {/Symbol m}_{model-1}(z) )'   !=- log10
               !MS_fields(6) = 'log_{10} 1+{/Symbol D}{/Symbol m}(z)_'                           !=- log10

               do k=1,2    !=- k=1 - absolutes, k=2 - residuals
                  do j=3,3

                     GNUfields(title)  = 'set title "'// trim(MS_fields(1+5*(k-1))) //' for different models"'
                      plot1 = 100
                     title1 = 101

                     do i=1,MS_real_model_count
                        col = 1+j+3*(i-1)

                        select case (k)
                           case (1)
                              GNUfields(ylabel) = 'set ylabel "' // trim(MS_fields(1)) // '"'
                              GNUfields(yrange) =  'set yrange [38:*]'

                              GNUfields (plot1+i*2) = ', "' // trim(slashfix(model_set_path)) // &
                                 '" u 1:' // trim( inttostr( col ) ) // ' w l ls ' // trim(inttostr(i))
                              GNUfields (plot1) = ', "' // trim(slashfix(GRB_catalog_path)) // &
                                 '" u 2:3:4 w yerrorb ls ' // trim(inttostr( MS_real_model_count+1 ))
                              GNUfields (plot1+MS_real_model_count*2+2) = ', "' // trim(slashfix(GRB_medians_path)) // &
                                 '" u 1:2:3 w yerrorb ls ' // trim(inttostr( MS_real_model_count+2 ))
                           case(2)
                              GNUfields(yrange) =  'set yrange [*:*]'
                              GNUfields(yrange) =  'set yrange [-2:4]'

                              GNUfields(ylabel) = 'set ylabel "' // trim(MS_fields(2)) // ' = ' // &
                                 trim(MS_fields(3)) // '"'

                              GNUfields (plot1+i*2) = ', "' // trim(slashfix(model_set_path)) // &
                                 '" u 1:(($' // trim( inttostr( col ) ) // ' - $' // &
                                 !'" u 1:(log10(10+$' // trim( inttostr( col ) ) // ' - $' // &   !=- log10
                                 trim(inttostr( 1+j+3*(n_LCDM-1) )) // ')) w l ls ' // trim(inttostr(i))
                              GNUfields (plot1) = ', "' // trim(slashfix(GRB_catalog_path)) // &
                                 !'" u 2:5:4 w yerrorb ls ' // trim(inttostr( MS_real_model_count+1 ))
                                 '" u 2:5 w p ls ' // trim(inttostr( MS_real_model_count+1 ))
                                 !'" u 2:(log10(10+$5)) w p ls ' // trim(inttostr( MS_real_model_count+1 ))   !=- log10
                              GNUfields (plot1+MS_real_model_count*2+2) = ', "' // trim(slashfix(GRB_medians_path)) // &
                                 '" u 1:5:3 w yerrorb ls ' // trim(inttostr( MS_real_model_count+2 ))
                           end select

                        GNUfields (title1+i*2) = ' title "model-' // trim(adjustl(inttostr(i))) // ':' // &   !=-
                           trim( model_set (i,2) ) // '"'
                        end do

                     GNUfields (plot1) = GNUfields (plot1)(2:len)

                     GNUfields (title1) = ' notitle "GRBs(Amati-193)"'
                     GNUfields (title1+MS_real_model_count*2+2) = ' title "GRBs_{medians}(Amati-193)"'

                     graph_name = trim(MS_fields(4+1*(k-1))) // 'all_models'
                        GNUfields(extention_out_figure)='#eps' ; call plot(model_set_path)
                     graph_name = trim(MS_fields(4+1*(k-1))) // 'all_models'
                        GNUfields(extention_out_figure)='#png' ; call plot(model_set_path)
                     enddo
                  enddo

            end subroutine MN_Letter_example

end module COSMOD_graphics
