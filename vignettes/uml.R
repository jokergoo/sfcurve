
nomnoml("



#direction: down
#fill:white
[sfc_sequence || + seq <factor>
                 + rot <integer>
                 + universe <character>
              |  + `\[`()
                 + `\[<-`()
                 + c()
                 + length()
                 + show()
                 + sfc_universe()
                 + sfc_is_compatible()
                 + sfc_segments()
                 + sfc_grob()
                 + plot()
                 + sfc_rotate()
                 + sfc_hflip()
                 + sfc_vflip()
                 + sfc_dflip()
                 + sfc_reverse()
                 + sfc_validate()]

[sfc_base || + letter <character>
             + preceeding <numeric>
             + succeeding <numeric>
             + in_direction <numeric>
             + out_direction <numeric>
             + primary <logical>
             + open <logical>
             + grob <grid::grob> 
          |  + show()
             + plot()
             + sfc_grob()
             + sfc_previous_point()
             + sfc_next_point()
             ]
[sfc_sequence] -> [sfc_unit]

[sfc_unit || + corner <integer>
           | + sfc_flip_unit()]
[sfc_base] o--> [sfc_rules]
[sfc_unit] o--> [sfc_rules]
[sfc_rules] o--> [sfc_nxn]
[sfc_seed] o--> [sfc_nxn]
[sfc_rules || + rules <a list of sfc_unit>
              + flip <list>
              + universe <character>
              + name <character>
              + bases <a list of sfc_base>
            | + show()
              + sfc_universe()
              + sfc_mode()
              + sfc_expand_by_rules()
              ]
[sfc_nxn || + seed <sfc_seed>
            + rules <sfc_rules>
            + expansion <integer>
            + level <integer>
            + n <integer>
         |  + sfc_apply()
            + sfc_level()
            + sfc_mode()
            + show()
            + sfc_flip_unit()
            + sfc_grob()
            + plot()
            + sfc_index()
            + `\[`()
            + sfc_reduce()
            ]
[sfc_2x2 | + sfc_expand()]
[sfc_3x3_peano | + sfc_expand()
                 + level1_unit_orientation()
                 + change_level1_unit_orientation()]
[sfc_3x3_meander | + sfc_expand()
                   + level1_unit_orientation()
                   + change_level1_unit_orientation()]
[sfc_4x4_meander || + type <integer>
                 |  + sfc_expand()]
[sfc_3x3_combined || + sfc_expand()]
[sfc_sequence] -> [sfc_nxn]
[sfc_sequence] -> [sfc_seed]
[sfc_nxn] -> [sfc_2x2]
[sfc_nxn] -> [sfc_3x3_peano]
[sfc_nxn] -> [sfc_3x3_meander]
[sfc_nxn] -> [sfc_3x3_combined]
[sfc_nxn] -> [sfc_4x4_meander]
[sfc_4x4_meander] -> [sfc_4x4_meander_1]
[sfc_4x4_meander] -> [sfc_4x4_meander_2]





")


