#How to figure out a schemer:
#Tracing types of Roman comedy in classical receptions through digital methods
#by Julia Jennifer Beine

#[1.] load necessary libraries: 
library(rdracor)
library(tidyverse)
library(igraph)
library(tidygraph)
library(data.table)
library(insight)
library(gt)
library(webshot2)
library(ggnetwork)
library(RColorBrewer)
library(visNetwork)
library(network)
library(scales)
library(gtExtras)
library(graphlayouts)
library(ggraph)
library(ggforce)

#[2.] get information on the version of the DraCor API
dracor_api_info()

#[3.] get information on the corpora
#[3.1.] get information on EngDraCor including the last update
meta <- get_dracor_meta()
##convert tibble to data frame
meta_df <- as.data.frame(meta)
show(meta_df)
##delete columns and rows not needed for quotation
filt_meta_df <- meta_df %>%
  filter(title == "English Drama Corpus") %>%
  select(-c(plays, characters, male, female, text, sp, stage,
            wordcount.text, wordcount.sp, wordcount.stage))
show(filt_meta_df)
##create table with gt
gt_filt_meta <- gt::gt(filt_meta_df)
show(gt_filt_meta)
##amend table
gt_filt_meta <-
  gt_filt_meta %>%
  ###add header
  tab_header(
    title = "Information on EngDraCor")
show(gt_filt_meta)
##export table
gt_filt_meta_png <-gt::gtsave(gt_filt_meta, "Info_EngDraCor.png",
                              vwidth = 2000,
                              vheight = 1000,
                              zoom = 10)

#[3.2.] get information on FreDraCor including the last update
meta <- get_dracor_meta()
##convert tibble to data frame
meta_df <- as.data.frame(meta)
show(meta_df)
##delete columns and rows not needed for quotation
filt_meta_df <- meta_df %>%
  filter(title == "French Drama Corpus") %>%
  select(-c(plays, characters, male, female, text, sp, stage,
            wordcount.text, wordcount.sp, wordcount.stage))
show(filt_meta_df)
##create table with gt
gt_filt_meta <- gt::gt(filt_meta_df)
show(gt_filt_meta)
##amend table
gt_filt_meta <-
  gt_filt_meta %>%
  ###add header
  tab_header(
    title = "Information on FreDraCor")
show(gt_filt_meta)
##export table
gt_filt_meta_png <-gt::gtsave(gt_filt_meta, "Info_FreDraCor.png",
                              vwidth = 2000,
                              vheight = 1000,
                              zoom = 10)

#[3.3.] get information on ItaDraCor including the last update
meta <- get_dracor_meta()
##convert tibble to data frame
meta_df <- as.data.frame(meta)
show(meta_df)
##delete columns and rows not needed for quotation
filt_meta_df <- meta_df %>%
  filter(title == "Italian Drama Corpus") %>%
  select(-c(plays, characters, male, female, text, sp, stage,
            wordcount.text, wordcount.sp, wordcount.stage))
show(filt_meta_df)
##create table with gt
gt_filt_meta <- gt::gt(filt_meta_df)
show(gt_filt_meta)
##amend table
gt_filt_meta <-
  gt_filt_meta %>%
  ###add header
  tab_header(
    title = "Information on ItaDraCor")
show(gt_filt_meta)
##export table
gt_filt_meta_png <-gt::gtsave(gt_filt_meta, "Info_ItaDraCor.png",
                              vwidth = 2000,
                              vheight = 1000,
                              zoom = 10)

#[4.] analyse the early modern English comedies (EngDraCor)

#[4.1.] Chapman, All Fools

##[4.1.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
fools <- get_play_metadata(play = "chapman-all-fools", 
                           corpus = "eng",
                           full_metadata = TRUE)
show(fools)
####create matrix with general values for the dramatic network
fools_gen_net_struc <- matrix(c(fools$size, fools$density,
                                fools$diameter, fools$averageClustering,
                                fools$averagePathLength,
                                fools$averageDegree),
                              ncol = 1, byrow = FALSE)
###convert matrix to data frame
fools_gen_net_struc_df <- as.data.frame(fools_gen_net_struc)
###specify columns and rows for the data frame
colnames(fools_gen_net_struc_df) <- c("value")
rownames(fools_gen_net_struc_df) <- c("size", "density", "diameter",
                                      "average clustering",
                                      "average path length", "average degree")
show(fools_gen_net_struc_df)
###create table with gt
gt_fools_gen_net_struc <- gt::gt(fools_gen_net_struc_df,
                                 rownames_to_stub = TRUE)
show(gt_fools_gen_net_struc)
####amend table
gt_fools_gen_net_struc <-
  gt_fools_gen_net_struc %>%
  #####add header
  tab_header(
    title = "All Fools",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_fools_gen_net_struc)
####export table
gtsave(gt_fools_gen_net_struc, "fools_gen_net_struc.png", zoom = 10)

##[4.1.2.] extract, calculate, and add character specific values
fools_coocur <- get_net_cooccur_igraph(play =
                                         "chapman-all-fools",
                                       corpus = "eng")
class(fools_coocur)
###calculate local clustering
fools_local_clustering <- transitivity(
  fools_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
fools_coocur <- set_vertex_attr(fools_coocur, "local_clustering",
                                value = fools_local_clustering)
###calculate triangles
fools_triangles <- count_triangles(fools_coocur)
###add triangles
fools_coocur <- set_vertex_attr(fools_coocur, "triangles",
                                value = fools_triangles)
###show data
show(fools_coocur)
class(fools_coocur)
###export as data frame
fools_char_spec_v_df <- as_data_frame(fools_coocur, what="vertices")
show(fools_char_spec_v_df)

##[4.1.3.] create two tables with character specific values

###[4.1.3.1.] create table with count-based measures for each character
####extract table with count-based measures for each character
fools_char_sp_df <- select(fools_char_spec_v_df,
                           c(numOfWords, numOfSpeechActs, numOfScenes))
show(fools_char_sp_df)
####arrange rows by number of scenes
fools_char_sp_df <- arrange(fools_char_sp_df,
                            desc(numOfWords),
                            desc(numOfSpeechActs))
show(fools_char_sp_df)
####create table with gt
gt_fools_char_sp <- gt::gt(fools_char_sp_df, rownames_to_stub = TRUE)
show(gt_fools_char_sp)
####layout table
gt_fools_char_sp <-
  gt_fools_char_sp %>%
  #####add header
  tab_header(
    title = "All Fools",
    subtitle = "count-based measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(numOfWords = "words",
             numOfSpeechActs = "speech acts",
             numOfScenes = "scenes") %>%
  #####bold row showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Rinaldo")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Rinaldo"))
#####colour the table: the higher the value, the darker the cell
gt_fools_char_sp <-
  gt_fools_char_sp %>%
  gt_color_rows(numOfWords:numOfScenes,
                palette = RColorBrewer::brewer.pal(5,"GnBu"))
#####show final layout
show(gt_fools_char_sp)
#####set margin to prevent cut off
gt_fools_char_sp <-
  gt_fools_char_sp %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_fools_char_sp)
####export table
gtsave(gt_fools_char_sp, "fools_char_sp.png", zoom = 10)

###[4.1.3.2.] create table with character specific network values
###extract table with character specific network values
fools_char_net_df <- select(fools_char_spec_v_df,
                            c(degree, weightedDegree,
                              closeness, betweenness,
                              local_clustering, triangles))
show(fools_char_net_df)
####arrange rows by degree
fools_char_net_df <- arrange(fools_char_net_df, 
                             desc(degree),
                             desc(weightedDegree))
show(fools_char_net_df)
####create table with gt
gt_fools_char_net <- gt::gt(fools_char_net_df, rownames_to_stub = TRUE)
show(gt_fools_char_net)
####layout table
gt_fools_char_net <-
  gt_fools_char_net %>%
  #####add header
  tab_header(
    title = "All Fools",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.")
#####colour the table
gt_fools_char_net <-
  gt_fools_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Rinaldo")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Rinaldo"))
####show final layout
show(gt_fools_char_net)
#####set margin to prevent cut off
gt_fools_char_net <-
  gt_fools_char_net %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_fools_char_net)
####export table
gtsave(gt_fools_char_net, "fools_char_net.png", zoom = 10)

##[4.1.4.] evaluate the data regarding the hypotheses of the paper

###[4.1.4.1.] create a vector with the scheming characters 
fools_selected_characters <- c("Rinaldo")
show(fools_selected_characters)

###[4.1.4.2.] evaluate the count-based data

####[4.1.4.2.1.] check if the number of words, the number of speech acts, and 
####the number of scenes of the schemer are among the three best ranked
####values respectively
fools_char_sp_df_eval_3 <-
  fools_char_sp_df %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(fools_char_sp_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
fools_char_sp_df_eval_3_schemer <- 
  fools_char_sp_df_eval_3 %>%
  filter(row.names(fools_char_sp_df_eval_3) 
         %in% fools_selected_characters)
show(fools_char_sp_df_eval_3_schemer)

###[4.1.4.3.] evaluate the network data

####[4.1.4.3.1.] check if the degree centrality, weighted degree centrality,
####closeness centrality, betweenness centrality, and the number of triangles
####of the schemer are among the three best ranked values respectively.
####check if the clustering coefficient of the schemer
####is among the three lowest values.
fools_char_net_df_eval_3 <-
  fools_char_net_df %>%
  #####reverse values for local clustering
  mutate(local_clustering = -local_clustering) %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(fools_char_net_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
fools_char_net_df_eval_3_schemer <- 
  fools_char_net_df_eval_3 %>%
  filter(row.names(fools_char_net_df_eval_3) 
         %in% fools_selected_characters)
show(fools_char_net_df_eval_3_schemer)

####[4.1.4.3.2.] check if the degree centrality of the schemer is above
####average
fools_char_net_df_eval_dc_av <-
  fools_char_net_df %>%
  #####check if degree centrality is above average and extract column
  transmute(fools_char_net_df$degree > fools$averageDegree)
show(fools_char_net_df_eval_dc_av)
#####rename column
fools_char_net_df_eval_dc_av <-
  fools_char_net_df_eval_dc_av %>%
  rename("d.c. > av. d.c." = 
           "fools_char_net_df$degree > fools$averageDegree")
show(fools_char_net_df_eval_dc_av)
#####filter the selected character via logical vector to choose the schemer
fools_char_net_df_eval_dc_av_schemer <-
  fools_char_net_df_eval_dc_av %>%
  filter(row.names(fools_char_net_df_eval_dc_av) %in% fools_selected_characters)
show(fools_char_net_df_eval_dc_av_schemer)

####[4.1.4.3.3.] check if the clustering coefficient of the schemer is below
####average
fools_char_net_df_eval_cc_av <-
  fools_char_net_df %>%
  #####check if clustering coefficient is below average and extract column
  transmute(fools_char_net_df$local_clustering < fools$averageClustering)
show(fools_char_net_df_eval_cc_av)
#####rename column
fools_char_net_df_eval_cc_av <-
  fools_char_net_df_eval_cc_av %>%
  rename("c.c. < av. c.c." 
         = "fools_char_net_df$local_clustering < fools$averageClustering")
show(fools_char_net_df_eval_cc_av)
#####filter the selected character via logical vector to choose the schemer
fools_char_net_df_eval_cc_av_schemer <-
  fools_char_net_df_eval_cc_av %>%
  filter(row.names(fools_char_net_df_eval_cc_av) %in% fools_selected_characters)
show(fools_char_net_df_eval_cc_av_schemer)

##[4.1.5.] create network graph

###[4.1.5.1.] layout network graph
###choose layout algorithm for graph
fools_coocur_layout <- create_layout(fools_coocur, layout = "stress")
###layout settings
fools_coocur_layout <-
  ggraph(fools_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(fools_coocur_layout)
###export graph
ggsave(fools_coocur_layout,
       file = "fools_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

#[4.2.] Heywood, The English Traveller

##[4.2.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
traveller <- get_play_metadata(play = "heywood-the-english-traveller", 
                               corpus = "eng",
                               full_metadata = TRUE)
show(traveller)
####create matrix with general values for the dramatic network
traveller_gen_net_struc <- matrix(c(traveller$size, traveller$density,
                                    traveller$diameter, traveller$averageClustering,
                                    traveller$averagePathLength,
                                    traveller$averageDegree),
                                  ncol = 1, byrow = FALSE)
###convert matrix to data frame
traveller_gen_net_struc_df <- as.data.frame(traveller_gen_net_struc)
###specify columns and rows for the data frame
colnames(traveller_gen_net_struc_df) <- c("value")
rownames(traveller_gen_net_struc_df) <- c("size", "density", "diameter",
                                          "average clustering",
                                          "average path length", "average degree")
show(traveller_gen_net_struc_df)
###create table with gt
gt_traveller_gen_net_struc <- gt::gt(traveller_gen_net_struc_df,
                                     rownames_to_stub = TRUE)
show(gt_traveller_gen_net_struc)
####amend table
gt_traveller_gen_net_struc <-
  gt_traveller_gen_net_struc %>%
  #####add header
  tab_header(
    title = "The English Traveller",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_traveller_gen_net_struc)
####export table
gtsave(gt_traveller_gen_net_struc, "traveller_gen_net_struc.png", zoom = 10)

##[4.2.2.] extract, calculate, and add character specific values
traveller_coocur <- get_net_cooccur_igraph(play =
                                             "heywood-the-english-traveller",
                                           corpus = "eng")
class(traveller_coocur)
###calculate local clustering
traveller_local_clustering <- transitivity(
  traveller_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
traveller_coocur <- set_vertex_attr(traveller_coocur, "local_clustering",
                                    value = traveller_local_clustering)
###calculate triangles
traveller_triangles <- count_triangles(traveller_coocur)
###add triangles
traveller_coocur <- set_vertex_attr(traveller_coocur, "triangles",
                                    value = traveller_triangles)
###show data
show(traveller_coocur)
class(traveller_coocur)
###export as data frame
traveller_char_spec_v_df <- as_data_frame(traveller_coocur, what="vertices")
show(traveller_char_spec_v_df)

##[4.2.3.] create two tables with character specific values

###[4.2.3.1.] create table with count-based measures for each character
####extract table with count-based measures for each character
traveller_char_sp_df <- select(traveller_char_spec_v_df,
                               c(numOfWords, numOfSpeechActs, numOfScenes))
show(traveller_char_sp_df)
####arrange rows by number of scenes
traveller_char_sp_df <- arrange(traveller_char_sp_df,
                                desc(numOfWords),
                                desc(numOfSpeechActs))
show(traveller_char_sp_df)
####create table with gt
gt_traveller_char_sp <- gt::gt(traveller_char_sp_df, rownames_to_stub = TRUE)
show(gt_traveller_char_sp)
####layout table
gt_traveller_char_sp <-
  gt_traveller_char_sp %>%
  #####add header
  tab_header(
    title = "The English Traveller",
    subtitle = "count-based measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(numOfWords = "words",
             numOfSpeechActs = "speech acts",
             numOfScenes = "scenes") %>%
  #####bold row showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Reignald")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Reignald"))
#####colour the table: the higher the value, the darker the cell
gt_traveller_char_sp <-
  gt_traveller_char_sp %>%
  gt_color_rows(numOfWords:numOfScenes,
                palette = RColorBrewer::brewer.pal(5,"GnBu"))
#####show final layout
show(gt_traveller_char_sp)
#####set margin to prevent cut off
gt_traveller_char_sp <-
  gt_traveller_char_sp %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_traveller_char_sp)
####export table
gtsave(gt_traveller_char_sp, "traveller_char_sp.png", zoom = 10)

###[4.2.3.2.] create table with character specific network values
###extract table with character specific network values
traveller_char_net_df <- select(traveller_char_spec_v_df,
                                c(degree, weightedDegree,
                                  closeness, betweenness,
                                  local_clustering, triangles))
show(traveller_char_net_df)
####arrange rows by degree
traveller_char_net_df <- arrange(traveller_char_net_df, 
                                 desc(degree),
                                 desc(weightedDegree))
show(traveller_char_net_df)
####create table with gt
gt_traveller_char_net <- gt::gt(traveller_char_net_df, rownames_to_stub = TRUE)
show(gt_traveller_char_net)
####layout table
gt_traveller_char_net <-
  gt_traveller_char_net %>%
  #####add header
  tab_header(
    title = "The English Traveller",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.")
#####colour the table
gt_traveller_char_net <-
  gt_traveller_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Reignald")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Reignald"))
####show final layout
show(gt_traveller_char_net)
#####set margin to prevent cut off
gt_traveller_char_net <-
  gt_traveller_char_net %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_traveller_char_net)
####export table
gtsave(gt_traveller_char_net, "traveller_char_net.png", zoom = 10)

##[4.2.4.] evaluate the data regarding the hypotheses of the paper

###[4.2.4.1.] create a vector with the scheming characters 
traveller_selected_characters <- c("Reignald")
show(traveller_selected_characters)

###[4.2.4.2.] evaluate the count-based data

####[4.2.4.2.1.] check if the number of words, the number of speech acts, and 
####the number of scenes of the schemer are among the three best ranked
####values respectively
traveller_char_sp_df_eval_3 <-
  traveller_char_sp_df %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(traveller_char_sp_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
traveller_char_sp_df_eval_3_schemer <- 
  traveller_char_sp_df_eval_3 %>%
  filter(row.names(traveller_char_sp_df_eval_3) 
         %in% traveller_selected_characters)
show(traveller_char_sp_df_eval_3_schemer)

###[4.2.4.3.] evaluate the network data

####[4.2.4.3.1.] check if the degree centrality, weighted degree centrality,
####closeness centrality, betweenness centrality, and the number of triangles
####of the schemer are among the three best ranked values respectively.
####check if the clustering coefficient of the schemer
####is among the three lowest values.
traveller_char_net_df_eval_3 <-
  traveller_char_net_df %>%
  #####reverse values for local clustering
  mutate(local_clustering = -local_clustering) %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(traveller_char_net_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
traveller_char_net_df_eval_3_schemer <- 
  traveller_char_net_df_eval_3 %>%
  filter(row.names(traveller_char_net_df_eval_3) 
         %in% traveller_selected_characters)
show(traveller_char_net_df_eval_3_schemer)

####[4.2.4.3.2.] check if the degree centrality of the schemer is above
####average
traveller_char_net_df_eval_dc_av <-
  traveller_char_net_df %>%
  #####check if degree centrality is above average and extract column
  transmute(traveller_char_net_df$degree > traveller$averageDegree)
show(traveller_char_net_df_eval_dc_av)
#####rename column
traveller_char_net_df_eval_dc_av <-
  traveller_char_net_df_eval_dc_av %>%
  rename("d.c. > av. d.c." = 
           "traveller_char_net_df$degree > traveller$averageDegree")
show(traveller_char_net_df_eval_dc_av)
#####filter the selected character via logical vector to choose the schemer
traveller_char_net_df_eval_dc_av_schemer <-
  traveller_char_net_df_eval_dc_av %>%
  filter(row.names(traveller_char_net_df_eval_dc_av) %in% traveller_selected_characters)
show(traveller_char_net_df_eval_dc_av_schemer)

####[4.2.4.3.3.] check if the clustering coefficient of the schemer is below
####average
traveller_char_net_df_eval_cc_av <-
  traveller_char_net_df %>%
  #####check if clustering coefficient is below average and extract column
  transmute(traveller_char_net_df$local_clustering < traveller$averageClustering)
show(traveller_char_net_df_eval_cc_av)
#####rename column
traveller_char_net_df_eval_cc_av <-
  traveller_char_net_df_eval_cc_av %>%
  rename("c.c. < av. c.c." 
         = "traveller_char_net_df$local_clustering < traveller$averageClustering")
show(traveller_char_net_df_eval_cc_av)
#####filter the selected character via logical vector to choose the schemer
traveller_char_net_df_eval_cc_av_schemer <-
  traveller_char_net_df_eval_cc_av %>%
  filter(row.names(traveller_char_net_df_eval_cc_av) %in% traveller_selected_characters)
show(traveller_char_net_df_eval_cc_av_schemer)

##[4.2.5.] create network graph

###[4.2.5.1.] layout network graph
###choose layout algorithm for graph
traveller_coocur_layout <- create_layout(traveller_coocur, layout = "stress")
###layout settings
traveller_coocur_layout <-
  ggraph(traveller_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(traveller_coocur_layout)
###export graph
ggsave(traveller_coocur_layout,
       file = "traveller_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

#[4.3.] Jonson, The Case is Altered

##[4.3.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
case <- get_play_metadata(play = "jonson-the-case-is-altered", 
                          corpus = "eng",
                          full_metadata = TRUE)
show(case)
####create matrix with general values for the dramatic network
case_gen_net_struc <- matrix(c(case$size, case$density,
                               case$diameter, case$averageClustering,
                               case$averagePathLength,
                               case$averageDegree),
                             ncol = 1, byrow = FALSE)
###convert matrix to data frame
case_gen_net_struc_df <- as.data.frame(case_gen_net_struc)
###specify columns and rows for the data frame
colnames(case_gen_net_struc_df) <- c("value")
rownames(case_gen_net_struc_df) <- c("size", "density", "diameter",
                                     "average clustering",
                                     "average path length", "average degree")
show(case_gen_net_struc_df)
###create table with gt
gt_case_gen_net_struc <- gt::gt(case_gen_net_struc_df,
                                rownames_to_stub = TRUE)
show(gt_case_gen_net_struc)
####amend table
gt_case_gen_net_struc <-
  gt_case_gen_net_struc %>%
  #####add header
  tab_header(
    title = "The Case is Altered",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_case_gen_net_struc)
####export table
gtsave(gt_case_gen_net_struc, "case_gen_net_struc.png", zoom = 10)

##[4.3.2.] extract, calculate, and add character specific values
case_coocur <- get_net_cooccur_igraph(play =
                                        "jonson-the-case-is-altered",
                                      corpus = "eng")
class(case_coocur)
###calculate local clustering
case_local_clustering <- transitivity(
  case_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
case_coocur <- set_vertex_attr(case_coocur, "local_clustering",
                               value = case_local_clustering)
###calculate triangles
case_triangles <- count_triangles(case_coocur)
###add triangles
case_coocur <- set_vertex_attr(case_coocur, "triangles",
                               value = case_triangles)
###show data
show(case_coocur)
class(case_coocur)
###export as data frame
case_char_spec_v_df <- as_data_frame(case_coocur, what="vertices")
show(case_char_spec_v_df)

##[4.3.3.] create two tables with character specific values

###[4.3.3.1.] create table with count-based measures for each character
####extract table with count-based measures for each character
case_char_sp_df <- select(case_char_spec_v_df,
                          c(numOfWords, numOfSpeechActs, numOfScenes))
show(case_char_sp_df)
####arrange rows by number of scenes
case_char_sp_df <- arrange(case_char_sp_df,
                           desc(numOfWords),
                           desc(numOfSpeechActs))
show(case_char_sp_df)
####create table with gt
gt_case_char_sp <- gt::gt(case_char_sp_df, rownames_to_stub = TRUE)
show(gt_case_char_sp)
####layout table
gt_case_char_sp <-
  gt_case_char_sp %>%
  #####add header
  tab_header(
    title = "The Case is Altered",
    subtitle = "count-based measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(numOfWords = "words",
             numOfSpeechActs = "speech acts",
             numOfScenes = "scenes") %>%
  #####bold row showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Camillo")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Camillo"))
#####colour the table: the higher the value, the darker the cell
gt_case_char_sp <-
  gt_case_char_sp %>%
  gt_color_rows(numOfWords:numOfScenes,
                palette = RColorBrewer::brewer.pal(5,"GnBu"))
#####show final layout
show(gt_case_char_sp)
#####set margin to prevent cut off
gt_case_char_sp <-
  gt_case_char_sp %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_case_char_sp)
####export table
gtsave(gt_case_char_sp, "case_char_sp.png", zoom = 10)

###[4.3.3.2.] create table with character specific network values
###extract table with character specific network values
case_char_net_df <- select(case_char_spec_v_df,
                           c(degree, weightedDegree,
                             closeness, betweenness,
                             local_clustering, triangles))
show(case_char_net_df)
####arrange rows by degree
case_char_net_df <- arrange(case_char_net_df, 
                            desc(degree),
                            desc(weightedDegree))
show(case_char_net_df)
####create table with gt
gt_case_char_net <- gt::gt(case_char_net_df, rownames_to_stub = TRUE)
show(gt_case_char_net)
####layout table
gt_case_char_net <-
  gt_case_char_net %>%
  #####add header
  tab_header(
    title = "The Case is Altered",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.")
#####colour the table
gt_case_char_net <-
  gt_case_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Camillo")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Camillo"))
####show final layout
show(gt_case_char_net)
#####set margin to prevent cut off
gt_case_char_net <-
  gt_case_char_net %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_case_char_net)
####export table
gtsave(gt_case_char_net, "case_char_net.png", zoom = 10)

##[4.3.4.] evaluate the data regarding the hypotheses of the paper

###[4.3.4.1.] create a vector with the scheming characters 
case_selected_characters <- c("Camillo")
show(case_selected_characters)

###[4.3.4.2.] evaluate the count-based data

####[4.3.4.2.1.] check if the number of words, the number of speech acts, and 
####the number of scenes of the schemer are among the three best ranked
####values respectively
case_char_sp_df_eval_3 <-
  case_char_sp_df %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(case_char_sp_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
case_char_sp_df_eval_3_schemer <- 
  case_char_sp_df_eval_3 %>%
  filter(row.names(case_char_sp_df_eval_3) 
         %in% case_selected_characters)
show(case_char_sp_df_eval_3_schemer)

###[4.3.4.3.] evaluate the network data

####[4.3.4.3.1.] check if the degree centrality, weighted degree centrality,
####closeness centrality, betweenness centrality, and the number of triangles
####of the schemer are among the three best ranked values respectively.
####check if the clustering coefficient of the schemer
####is among the three lowest values.
case_char_net_df_eval_3 <-
  case_char_net_df %>%
  #####reverse values for local clustering
  mutate(local_clustering = -local_clustering) %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(case_char_net_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
case_char_net_df_eval_3_schemer <- 
  case_char_net_df_eval_3 %>%
  filter(row.names(case_char_net_df_eval_3) 
         %in% case_selected_characters)
show(case_char_net_df_eval_3_schemer)

####[4.3.4.3.2.] check if the degree centrality of the schemer is above
####average
case_char_net_df_eval_dc_av <-
  case_char_net_df %>%
  #####check if degree centrality is above average and extract column
  transmute(case_char_net_df$degree > case$averageDegree)
show(case_char_net_df_eval_dc_av)
#####rename column
case_char_net_df_eval_dc_av <-
  case_char_net_df_eval_dc_av %>%
  rename("d.c. > av. d.c." = 
           "case_char_net_df$degree > case$averageDegree")
show(case_char_net_df_eval_dc_av)
#####filter the selected character via logical vector to choose the schemer
case_char_net_df_eval_dc_av_schemer <-
  case_char_net_df_eval_dc_av %>%
  filter(row.names(case_char_net_df_eval_dc_av) %in% case_selected_characters)
show(case_char_net_df_eval_dc_av_schemer)

####[4.3.4.3.3.] check if the clustering coefficient of the schemer is below
####average
case_char_net_df_eval_cc_av <-
  case_char_net_df %>%
  #####check if clustering coefficient is below average and extract column
  transmute(case_char_net_df$local_clustering < case$averageClustering)
show(case_char_net_df_eval_cc_av)
#####rename column
case_char_net_df_eval_cc_av <-
  case_char_net_df_eval_cc_av %>%
  rename("c.c. < av. c.c." 
         = "case_char_net_df$local_clustering < case$averageClustering")
show(case_char_net_df_eval_cc_av)
#####filter the selected character via logical vector to choose the schemer
case_char_net_df_eval_cc_av_schemer <-
  case_char_net_df_eval_cc_av %>%
  filter(row.names(case_char_net_df_eval_cc_av) %in% case_selected_characters)
show(case_char_net_df_eval_cc_av_schemer)

##[4.3.5.] create network graph

###[4.3.5.1.] layout network graph
###choose layout algorithm for graph
case_coocur_layout <- create_layout(case_coocur, layout = "stress")
###layout settings
case_coocur_layout <-
  ggraph(case_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(case_coocur_layout)
###export graph
ggsave(case_coocur_layout,
       file = "case_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

#[4.4.] Jonson, Every Man in His Humour

##[4.4.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
man <- get_play_metadata(play = "jonson-every-man-in-his-humour", 
                         corpus = "eng",
                         full_metadata = TRUE)
show(man)
####create matrix with general values for the dramatic network
man_gen_net_struc <- matrix(c(man$size, man$density,
                              man$diameter, man$averageClustering,
                              man$averagePathLength,
                              man$averageDegree),
                            ncol = 1, byrow = FALSE)
###convert matrix to data frame
man_gen_net_struc_df <- as.data.frame(man_gen_net_struc)
###specify columns and rows for the data frame
colnames(man_gen_net_struc_df) <- c("value")
rownames(man_gen_net_struc_df) <- c("size", "density", "diameter",
                                    "average clustering",
                                    "average path length", "average degree")
show(man_gen_net_struc_df)
###create table with gt
gt_man_gen_net_struc <- gt::gt(man_gen_net_struc_df,
                               rownames_to_stub = TRUE)
show(gt_man_gen_net_struc)
####amend table
gt_man_gen_net_struc <-
  gt_man_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Every Man in His Humour",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_man_gen_net_struc)
####export table
gtsave(gt_man_gen_net_struc, "man_gen_net_struc.png", zoom = 10)

##[4.4.2.] extract, calculate, and add character specific values
man_coocur <- get_net_cooccur_igraph(play =
                                       "jonson-every-man-in-his-humour",
                                     corpus = "eng")
class(man_coocur)
###calculate local clustering
man_local_clustering <- transitivity(
  man_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
man_coocur <- set_vertex_attr(man_coocur, "local_clustering",
                              value = man_local_clustering)
###calculate triangles
man_triangles <- count_triangles(man_coocur)
###add triangles
man_coocur <- set_vertex_attr(man_coocur, "triangles",
                              value = man_triangles)
###show data
show(man_coocur)
class(man_coocur)
###export as data frame
man_char_spec_v_df <- as_data_frame(man_coocur, what="vertices")
show(man_char_spec_v_df)

##[4.4.3.] create two tables with character specific values

###[4.4.3.1.] create table with count-based measures for each character
####extract table with count-based measures for each character
man_char_sp_df <- select(man_char_spec_v_df,
                         c(numOfWords, numOfSpeechActs, numOfScenes))
show(man_char_sp_df)
####arrange rows by number of scenes
man_char_sp_df <- arrange(man_char_sp_df,
                          desc(numOfWords),
                          desc(numOfSpeechActs))
show(man_char_sp_df)
####create table with gt
gt_man_char_sp <- gt::gt(man_char_sp_df, rownames_to_stub = TRUE)
show(gt_man_char_sp)
####layout table
gt_man_char_sp <-
  gt_man_char_sp %>%
  #####add header
  tab_header(
    title = "Every Man in His Humour",
    subtitle = "count-based measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(numOfWords = "words",
             numOfSpeechActs = "speech acts",
             numOfScenes = "scenes") %>%
  #####bold row showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Brainworm")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Brainworm"))
#####colour the table: the higher the value, the darker the cell
gt_man_char_sp <-
  gt_man_char_sp %>%
  gt_color_rows(numOfWords:numOfScenes,
                palette = RColorBrewer::brewer.pal(5,"GnBu"))
#####show final layout
show(gt_man_char_sp)
#####set margin to prevent cut off
gt_man_char_sp <-
  gt_man_char_sp %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_man_char_sp)
####export table
gtsave(gt_man_char_sp, "man_char_sp.png", zoom = 10)

###[4.4.3.2.] create table with character specific network values
###extract table with character specific network values
man_char_net_df <- select(man_char_spec_v_df,
                          c(degree, weightedDegree,
                            closeness, betweenness,
                            local_clustering, triangles))
show(man_char_net_df)
####arrange rows by degree
man_char_net_df <- arrange(man_char_net_df, 
                           desc(degree),
                           desc(weightedDegree))
show(man_char_net_df)
####create table with gt
gt_man_char_net <- gt::gt(man_char_net_df, rownames_to_stub = TRUE)
show(gt_man_char_net)
####layout table
gt_man_char_net <-
  gt_man_char_net %>%
  #####add header
  tab_header(
    title = "Every Man in His Humour",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.")
#####colour the table
gt_man_char_net <-
  gt_man_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Brainworm")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Brainworm"))
####show final layout
show(gt_man_char_net)
#####set margin to prevent cut off
gt_man_char_net <-
  gt_man_char_net %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_man_char_net)
####export table
gtsave(gt_man_char_net, "man_char_net.png", zoom = 10)

##[4.4.4.] evaluate the data regarding the hypotheses of the paper

###[4.4.4.1.] create a vector with the scheming characters 
man_selected_characters <- c("Brainworm")
show(man_selected_characters)

###[4.4.4.2.] evaluate the count-based data

####[4.4.4.2.1.] check if the number of words, the number of speech acts, and 
####the number of scenes of the schemer are among the three best ranked
####values respectively
man_char_sp_df_eval_3 <-
  man_char_sp_df %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(man_char_sp_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
man_char_sp_df_eval_3_schemer <- 
  man_char_sp_df_eval_3 %>%
  filter(row.names(man_char_sp_df_eval_3) 
         %in% man_selected_characters)
show(man_char_sp_df_eval_3_schemer)

###[4.4.4.3.] evaluate the network data

####[4.4.4.3.1.] check if the degree centrality, weighted degree centrality,
####closeness centrality, betweenness centrality, and the number of triangles
####of the schemer are among the three best ranked values respectively.
####check if the clustering coefficient of the schemer
####is among the three lowest values.
man_char_net_df_eval_3 <-
  man_char_net_df %>%
  #####reverse values for local clustering
  mutate(local_clustering = -local_clustering) %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(man_char_net_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
man_char_net_df_eval_3_schemer <- 
  man_char_net_df_eval_3 %>%
  filter(row.names(man_char_net_df_eval_3) 
         %in% man_selected_characters)
show(man_char_net_df_eval_3_schemer)

####[4.4.4.3.2.] check if the degree centrality of the schemer is above
####average
man_char_net_df_eval_dc_av <-
  man_char_net_df %>%
  #####check if degree centrality is above average and extract column
  transmute(man_char_net_df$degree > man$averageDegree)
show(man_char_net_df_eval_dc_av)
#####rename column
man_char_net_df_eval_dc_av <-
  man_char_net_df_eval_dc_av %>%
  rename("d.c. > av. d.c." = 
           "man_char_net_df$degree > man$averageDegree")
show(man_char_net_df_eval_dc_av)
#####filter the selected character via logical vector to choose the schemer
man_char_net_df_eval_dc_av_schemer <-
  man_char_net_df_eval_dc_av %>%
  filter(row.names(man_char_net_df_eval_dc_av) %in% man_selected_characters)
show(man_char_net_df_eval_dc_av_schemer)

####[4.4.4.3.3.] check if the clustering coefficient of the schemer is below
####average
man_char_net_df_eval_cc_av <-
  man_char_net_df %>%
  #####check if clustering coefficient is below average and extract column
  transmute(man_char_net_df$local_clustering < man$averageClustering)
show(man_char_net_df_eval_cc_av)
#####rename column
man_char_net_df_eval_cc_av <-
  man_char_net_df_eval_cc_av %>%
  rename("c.c. < av. c.c." 
         = "man_char_net_df$local_clustering < man$averageClustering")
show(man_char_net_df_eval_cc_av)
#####filter the selected character via logical vector to choose the schemer
man_char_net_df_eval_cc_av_schemer <-
  man_char_net_df_eval_cc_av %>%
  filter(row.names(man_char_net_df_eval_cc_av) %in% man_selected_characters)
show(man_char_net_df_eval_cc_av_schemer)

##[4.4.5.] create network graph

###[4.4.5.1.] layout network graph
###choose layout algorithm for graph
man_coocur_layout <- create_layout(man_coocur, layout = "stress")
###layout settings
man_coocur_layout <-
  ggraph(man_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(man_coocur_layout)
###export graph
ggsave(man_coocur_layout,
       file = "man_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

#[4.5.] Jonson, Volpone

##[4.5.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
volpone <- get_play_metadata(play = "jonson-volpone", 
                             corpus = "eng",
                             full_metadata = TRUE)
show(volpone)
####create matrix with general values for the dramatic network
volpone_gen_net_struc <- matrix(c(volpone$size, volpone$density,
                                  volpone$diameter, volpone$averageClustering,
                                  volpone$averagePathLength,
                                  volpone$averageDegree),
                                ncol = 1, byrow = FALSE)
###convert matrix to data frame
volpone_gen_net_struc_df <- as.data.frame(volpone_gen_net_struc)
###specify columns and rows for the data frame
colnames(volpone_gen_net_struc_df) <- c("value")
rownames(volpone_gen_net_struc_df) <- c("size", "density", "diameter",
                                        "average clustering",
                                        "average path length", "average degree")
show(volpone_gen_net_struc_df)
###create table with gt
gt_volpone_gen_net_struc <- gt::gt(volpone_gen_net_struc_df,
                                   rownames_to_stub = TRUE)
show(gt_volpone_gen_net_struc)
####amend table
gt_volpone_gen_net_struc <-
  gt_volpone_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Volpone",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_volpone_gen_net_struc)
####export table
gtsave(gt_volpone_gen_net_struc, "volpone_gen_net_struc.png", zoom = 10)

##[4.5.2.] extract, calculate, and add character specific values
volpone_coocur <- get_net_cooccur_igraph(play =
                                           "jonson-volpone",
                                         corpus = "eng")
class(volpone_coocur)
###calculate local clustering
volpone_local_clustering <- transitivity(
  volpone_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
volpone_coocur <- set_vertex_attr(volpone_coocur, "local_clustering",
                                  value = volpone_local_clustering)
###calculate triangles
volpone_triangles <- count_triangles(volpone_coocur)
###add triangles
volpone_coocur <- set_vertex_attr(volpone_coocur, "triangles",
                                  value = volpone_triangles)
###show data
show(volpone_coocur)
class(volpone_coocur)
###export as data frame
volpone_char_spec_v_df <- as_data_frame(volpone_coocur, what="vertices")
show(volpone_char_spec_v_df)

##[4.5.3.] create two tables with character specific values

###[4.5.3.1.] create table with count-based measures for each character
####extract table with count-based measures for each character
volpone_char_sp_df <- select(volpone_char_spec_v_df,
                             c(numOfWords, numOfSpeechActs, numOfScenes))
show(volpone_char_sp_df)
####arrange rows by number of scenes
volpone_char_sp_df <- arrange(volpone_char_sp_df,
                              desc(numOfWords),
                              desc(numOfSpeechActs))
show(volpone_char_sp_df)
####create table with gt
gt_volpone_char_sp <- gt::gt(volpone_char_sp_df, rownames_to_stub = TRUE)
show(gt_volpone_char_sp)
####layout table
gt_volpone_char_sp <-
  gt_volpone_char_sp %>%
  #####add header
  tab_header(
    title = "Volpone",
    subtitle = "count-based measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(numOfWords = "words",
             numOfSpeechActs = "speech acts",
             numOfScenes = "scenes") %>%
  #####bold row showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Mosca")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Mosca"))
#####colour the table: the higher the value, the darker the cell
gt_volpone_char_sp <-
  gt_volpone_char_sp %>%
  gt_color_rows(numOfWords:numOfScenes,
                palette = RColorBrewer::brewer.pal(5,"GnBu"))
#####show final layout
show(gt_volpone_char_sp)
#####set margin to prevent cut off
gt_volpone_char_sp <-
  gt_volpone_char_sp %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_volpone_char_sp)
####export table
gtsave(gt_volpone_char_sp, "volpone_char_sp.png", zoom = 10)

###[4.5.3.2.] create table with character specific network values
###extract table with character specific network values
volpone_char_net_df <- select(volpone_char_spec_v_df,
                              c(degree, weightedDegree,
                                closeness, betweenness,
                                local_clustering, triangles))
show(volpone_char_net_df)
####arrange rows by degree
volpone_char_net_df <- arrange(volpone_char_net_df, 
                               desc(degree),
                               desc(weightedDegree))
show(volpone_char_net_df)
####create table with gt
gt_volpone_char_net <- gt::gt(volpone_char_net_df, rownames_to_stub = TRUE)
show(gt_volpone_char_net)
####layout table
gt_volpone_char_net <-
  gt_volpone_char_net %>%
  #####add header
  tab_header(
    title = "Volpone",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.")
#####colour the table
gt_volpone_char_net <-
  gt_volpone_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Mosca")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Mosca"))
####show final layout
show(gt_volpone_char_net)
#####set margin to prevent cut off
gt_volpone_char_net <-
  gt_volpone_char_net %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_volpone_char_net)
####export table
gtsave(gt_volpone_char_net, "volpone_char_net.png", zoom = 10)

##[4.5.4.] evaluate the data regarding the hypotheses of the paper

###[4.5.4.1.] create a vector with the scheming characters 
volpone_selected_characters <- c("Mosca")
show(volpone_selected_characters)

###[4.5.4.2.] evaluate the count-based data

####[4.5.4.2.1.] check if the number of words, the number of speech acts, and 
####the number of scenes of the schemer are among the three best ranked
####values respectively
volpone_char_sp_df_eval_3 <-
  volpone_char_sp_df %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(volpone_char_sp_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
volpone_char_sp_df_eval_3_schemer <- 
  volpone_char_sp_df_eval_3 %>%
  filter(row.names(volpone_char_sp_df_eval_3) 
         %in% volpone_selected_characters)
show(volpone_char_sp_df_eval_3_schemer)

###[4.5.4.3.] evaluate the network data

####[4.5.4.3.1.] check if the degree centrality, weighted degree centrality,
####closeness centrality, betweenness centrality, and the number of triangles
####of the schemer are among the three best ranked values respectively.
####check if the clustering coefficient of the schemer
####is among the three lowest values.
volpone_char_net_df_eval_3 <-
  volpone_char_net_df %>%
  #####reverse values for local clustering
  mutate(local_clustering = -local_clustering) %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(volpone_char_net_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
volpone_char_net_df_eval_3_schemer <- 
  volpone_char_net_df_eval_3 %>%
  filter(row.names(volpone_char_net_df_eval_3) 
         %in% volpone_selected_characters)
show(volpone_char_net_df_eval_3_schemer)

####[4.5.4.3.2.] check if the degree centrality of the schemer is above
####average
volpone_char_net_df_eval_dc_av <-
  volpone_char_net_df %>%
  #####check if degree centrality is above average and extract column
  transmute(volpone_char_net_df$degree > volpone$averageDegree)
show(volpone_char_net_df_eval_dc_av)
#####rename column
volpone_char_net_df_eval_dc_av <-
  volpone_char_net_df_eval_dc_av %>%
  rename("d.c. > av. d.c." = 
           "volpone_char_net_df$degree > volpone$averageDegree")
show(volpone_char_net_df_eval_dc_av)
#####filter the selected character via logical vector to choose the schemer
volpone_char_net_df_eval_dc_av_schemer <-
  volpone_char_net_df_eval_dc_av %>%
  filter(row.names(volpone_char_net_df_eval_dc_av) %in% volpone_selected_characters)
show(volpone_char_net_df_eval_dc_av_schemer)

####[4.5.4.3.3.] check if the clustering coefficient of the schemer is below
####average
volpone_char_net_df_eval_cc_av <-
  volpone_char_net_df %>%
  #####check if clustering coefficient is below average and extract column
  transmute(volpone_char_net_df$local_clustering < volpone$averageClustering)
show(volpone_char_net_df_eval_cc_av)
#####rename column
volpone_char_net_df_eval_cc_av <-
  volpone_char_net_df_eval_cc_av %>%
  rename("c.c. < av. c.c." 
         = "volpone_char_net_df$local_clustering < volpone$averageClustering")
show(volpone_char_net_df_eval_cc_av)
#####filter the selected character via logical vector to choose the schemer
volpone_char_net_df_eval_cc_av_schemer <-
  volpone_char_net_df_eval_cc_av %>%
  filter(row.names(volpone_char_net_df_eval_cc_av) %in% volpone_selected_characters)
show(volpone_char_net_df_eval_cc_av_schemer)

##[4.5.5.] create network graph

###[4.5.5.1.] layout network graph
###choose layout algorithm for graph
volpone_coocur_layout <- create_layout(volpone_coocur, layout = "stress")
###layout settings
volpone_coocur_layout <-
  ggraph(volpone_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(volpone_coocur_layout)
###export graph
ggsave(volpone_coocur_layout,
       file = "volpone_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

#[4.6.] Jonson, The Alchemist

##[4.6.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
alchemist <- get_play_metadata(play = "jonson-the-alchemist", 
                               corpus = "eng",
                               full_metadata = TRUE)
show(alchemist)
####create matrix with general values for the dramatic network
alchemist_gen_net_struc <- matrix(c(alchemist$size, alchemist$density,
                                    alchemist$diameter, alchemist$averageClustering,
                                    alchemist$averagePathLength,
                                    alchemist$averageDegree),
                                  ncol = 1, byrow = FALSE)
###convert matrix to data frame
alchemist_gen_net_struc_df <- as.data.frame(alchemist_gen_net_struc)
###specify columns and rows for the data frame
colnames(alchemist_gen_net_struc_df) <- c("value")
rownames(alchemist_gen_net_struc_df) <- c("size", "density", "diameter",
                                          "average clustering",
                                          "average path length", "average degree")
show(alchemist_gen_net_struc_df)
###create table with gt
gt_alchemist_gen_net_struc <- gt::gt(alchemist_gen_net_struc_df,
                                     rownames_to_stub = TRUE)
show(gt_alchemist_gen_net_struc)
####amend table
gt_alchemist_gen_net_struc <-
  gt_alchemist_gen_net_struc %>%
  #####add header
  tab_header(
    title = "The Alchemist",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_alchemist_gen_net_struc)
####export table
gtsave(gt_alchemist_gen_net_struc, "alchemist_gen_net_struc.png", zoom = 10)

##[4.6.2.] extract, calculate, and add character specific values
alchemist_coocur <- get_net_cooccur_igraph(play =
                                             "jonson-the-alchemist",
                                           corpus = "eng")
class(alchemist_coocur)
###calculate local clustering
alchemist_local_clustering <- transitivity(
  alchemist_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
alchemist_coocur <- set_vertex_attr(alchemist_coocur, "local_clustering",
                                    value = alchemist_local_clustering)
###calculate triangles
alchemist_triangles <- count_triangles(alchemist_coocur)
###add triangles
alchemist_coocur <- set_vertex_attr(alchemist_coocur, "triangles",
                                    value = alchemist_triangles)
###show data
show(alchemist_coocur)
class(alchemist_coocur)
###export as data frame
alchemist_char_spec_v_df <- as_data_frame(alchemist_coocur, what="vertices")
show(alchemist_char_spec_v_df)

##[4.6.3.] create two tables with character specific values

###[4.6.3.1.] create table with count-based measures for each character
####extract table with count-based measures for each character
alchemist_char_sp_df <- select(alchemist_char_spec_v_df,
                               c(numOfWords, numOfSpeechActs, numOfScenes))
show(alchemist_char_sp_df)
####arrange rows by number of scenes
alchemist_char_sp_df <- arrange(alchemist_char_sp_df,
                                desc(numOfWords),
                                desc(numOfSpeechActs))
show(alchemist_char_sp_df)
####create table with gt
gt_alchemist_char_sp <- gt::gt(alchemist_char_sp_df, rownames_to_stub = TRUE)
show(gt_alchemist_char_sp)
####layout table
gt_alchemist_char_sp <-
  gt_alchemist_char_sp %>%
  #####add header
  tab_header(
    title = "The Alchemist",
    subtitle = "count-based measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(numOfWords = "words",
             numOfSpeechActs = "speech acts",
             numOfScenes = "scenes") %>%
  #####bold row showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Face")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Face"))
#####colour the table: the higher the value, the darker the cell
gt_alchemist_char_sp <-
  gt_alchemist_char_sp %>%
  gt_color_rows(numOfWords:numOfScenes,
                palette = RColorBrewer::brewer.pal(5,"GnBu"))
#####show final layout
show(gt_alchemist_char_sp)
#####set margin to prevent cut off
gt_alchemist_char_sp <-
  gt_alchemist_char_sp %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_alchemist_char_sp)
####export table
gtsave(gt_alchemist_char_sp, "alchemist_char_sp.png", zoom = 10)

###[4.6.3.2.] create table with character specific network values
###extract table with character specific network values
alchemist_char_net_df <- select(alchemist_char_spec_v_df,
                                c(degree, weightedDegree,
                                  closeness, betweenness,
                                  local_clustering, triangles))
show(alchemist_char_net_df)
####arrange rows by degree
alchemist_char_net_df <- arrange(alchemist_char_net_df, 
                                 desc(degree),
                                 desc(weightedDegree))
show(alchemist_char_net_df)
####create table with gt
gt_alchemist_char_net <- gt::gt(alchemist_char_net_df, rownames_to_stub = TRUE)
show(gt_alchemist_char_net)
####layout table
gt_alchemist_char_net <-
  gt_alchemist_char_net %>%
  #####add header
  tab_header(
    title = "The Alchemist",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.")
#####colour the table
gt_alchemist_char_net <-
  gt_alchemist_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Face")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Face"))
####show final layout
show(gt_alchemist_char_net)
#####set margin to prevent cut off
gt_alchemist_char_net <-
  gt_alchemist_char_net %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_alchemist_char_net)
####export table
gtsave(gt_alchemist_char_net, "alchemist_char_net.png", zoom = 10)

##[4.6.4.] evaluate the data regarding the hypotheses of the paper

###[4.6.4.1.] create a vector with the scheming characters 
alchemist_selected_characters <- c("Face")
show(alchemist_selected_characters)

###[4.6.4.2.] evaluate the count-based data

####[4.6.4.2.1.] check if the number of words, the number of speech acts, and 
####the number of scenes of the schemer are among the three best ranked
####values respectively
alchemist_char_sp_df_eval_3 <-
  alchemist_char_sp_df %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(alchemist_char_sp_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
alchemist_char_sp_df_eval_3_schemer <- 
  alchemist_char_sp_df_eval_3 %>%
  filter(row.names(alchemist_char_sp_df_eval_3) 
         %in% alchemist_selected_characters)
show(alchemist_char_sp_df_eval_3_schemer)

###[4.6.4.3.] evaluate the network data

####[4.6.4.3.1.] check if the degree centrality, weighted degree centrality,
####closeness centrality, betweenness centrality, and the number of triangles
####of the schemer are among the three best ranked values respectively.
####check if the clustering coefficient of the schemer
####is among the three lowest values.
alchemist_char_net_df_eval_3 <-
  alchemist_char_net_df %>%
  #####reverse values for local clustering
  mutate(local_clustering = -local_clustering) %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(alchemist_char_net_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
alchemist_char_net_df_eval_3_schemer <- 
  alchemist_char_net_df_eval_3 %>%
  filter(row.names(alchemist_char_net_df_eval_3) 
         %in% alchemist_selected_characters)
show(alchemist_char_net_df_eval_3_schemer)

####[4.6.4.3.2.] check if the degree centrality of the schemer is above
####average
alchemist_char_net_df_eval_dc_av <-
  alchemist_char_net_df %>%
  #####check if degree centrality is above average and extract column
  transmute(alchemist_char_net_df$degree > alchemist$averageDegree)
show(alchemist_char_net_df_eval_dc_av)
#####rename column
alchemist_char_net_df_eval_dc_av <-
  alchemist_char_net_df_eval_dc_av %>%
  rename("d.c. > av. d.c." = 
           "alchemist_char_net_df$degree > alchemist$averageDegree")
show(alchemist_char_net_df_eval_dc_av)
#####filter the selected character via logical vector to choose the schemer
alchemist_char_net_df_eval_dc_av_schemer <-
  alchemist_char_net_df_eval_dc_av %>%
  filter(row.names(alchemist_char_net_df_eval_dc_av) %in% alchemist_selected_characters)
show(alchemist_char_net_df_eval_dc_av_schemer)

####[4.6.4.3.3.] check if the clustering coefficient of the schemer is below
####average
alchemist_char_net_df_eval_cc_av <-
  alchemist_char_net_df %>%
  #####check if clustering coefficient is below average and extract column
  transmute(alchemist_char_net_df$local_clustering < alchemist$averageClustering)
show(alchemist_char_net_df_eval_cc_av)
#####rename column
alchemist_char_net_df_eval_cc_av <-
  alchemist_char_net_df_eval_cc_av %>%
  rename("c.c. < av. c.c." 
         = "alchemist_char_net_df$local_clustering < alchemist$averageClustering")
show(alchemist_char_net_df_eval_cc_av)
#####filter the selected character via logical vector to choose the schemer
alchemist_char_net_df_eval_cc_av_schemer <-
  alchemist_char_net_df_eval_cc_av %>%
  filter(row.names(alchemist_char_net_df_eval_cc_av) %in% alchemist_selected_characters)
show(alchemist_char_net_df_eval_cc_av_schemer)

##[4.6.5.] create network graph

###[4.6.5.1.] layout network graph
###choose layout algorithm for graph
alchemist_coocur_layout <- create_layout(alchemist_coocur, layout = "stress")
###layout settings
alchemist_coocur_layout <-
  ggraph(alchemist_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(alchemist_coocur_layout)
###export graph
ggsave(alchemist_coocur_layout,
       file = "alchemist_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

#[4.7.] Udall, Ralph Roister Doister

##[4.7.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
ralph <- get_play_metadata(play = "udall-ralph-roister-doister", 
                           corpus = "eng",
                           full_metadata = TRUE)
show(ralph)
####create matrix with general values for the dramatic network
ralph_gen_net_struc <- matrix(c(ralph$size, ralph$density,
                                ralph$diameter, ralph$averageClustering,
                                ralph$averagePathLength,
                                ralph$averageDegree),
                              ncol = 1, byrow = FALSE)
###convert matrix to data frame
ralph_gen_net_struc_df <- as.data.frame(ralph_gen_net_struc)
###specify columns and rows for the data frame
colnames(ralph_gen_net_struc_df) <- c("value")
rownames(ralph_gen_net_struc_df) <- c("size", "density", "diameter",
                                      "average clustering",
                                      "average path length", "average degree")
show(ralph_gen_net_struc_df)
###create table with gt
gt_ralph_gen_net_struc <- gt::gt(ralph_gen_net_struc_df,
                                 rownames_to_stub = TRUE)
show(gt_ralph_gen_net_struc)
####amend table
gt_ralph_gen_net_struc <-
  gt_ralph_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Ralph Roister Doister",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_ralph_gen_net_struc)
####export table
gtsave(gt_ralph_gen_net_struc, "ralph_gen_net_struc.png", zoom = 10)

##[4.7.2.] extract, calculate, and add character specific values
ralph_coocur <- get_net_cooccur_igraph(play =
                                         "udall-ralph-roister-doister",
                                       corpus = "eng")
class(ralph_coocur)
###calculate local clustering
ralph_local_clustering <- transitivity(
  ralph_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
ralph_coocur <- set_vertex_attr(ralph_coocur, "local_clustering",
                                value = ralph_local_clustering)
###calculate triangles
ralph_triangles <- count_triangles(ralph_coocur)
###add triangles
ralph_coocur <- set_vertex_attr(ralph_coocur, "triangles",
                                value = ralph_triangles)
###show data
show(ralph_coocur)
class(ralph_coocur)
###export as data frame
ralph_char_spec_v_df <- as_data_frame(ralph_coocur, what="vertices")
show(ralph_char_spec_v_df)

##[4.7.3.] create two tables with character specific values

###[4.7.3.1.] create table with count-based measures for each character
####extract table with count-based measures for each character
ralph_char_sp_df <- select(ralph_char_spec_v_df,
                           c(numOfWords, numOfSpeechActs, numOfScenes))
show(ralph_char_sp_df)
####arrange rows by number of scenes
ralph_char_sp_df <- arrange(ralph_char_sp_df,
                            desc(numOfWords),
                            desc(numOfSpeechActs))
show(ralph_char_sp_df)
####create table with gt
gt_ralph_char_sp <- gt::gt(ralph_char_sp_df, rownames_to_stub = TRUE)
show(gt_ralph_char_sp)
####layout table
gt_ralph_char_sp <-
  gt_ralph_char_sp %>%
  #####add header
  tab_header(
    title = "Ralph Roister Doister",
    subtitle = "count-based measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(numOfWords = "words",
             numOfSpeechActs = "speech acts",
             numOfScenes = "scenes") %>%
  #####bold row showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Merrygreek")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Merrygreek"))
#####colour the table: the higher the value, the darker the cell
gt_ralph_char_sp <-
  gt_ralph_char_sp %>%
  gt_color_rows(numOfWords:numOfScenes,
                palette = RColorBrewer::brewer.pal(5,"GnBu"))
#####show final layout
show(gt_ralph_char_sp)
#####set margin to prevent cut off
gt_ralph_char_sp <-
  gt_ralph_char_sp %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_ralph_char_sp)
####export table
gtsave(gt_ralph_char_sp, "ralph_char_sp.png", zoom = 10)

###[4.7.3.2.] create table with character specific network values
###extract table with character specific network values
ralph_char_net_df <- select(ralph_char_spec_v_df,
                            c(degree, weightedDegree,
                              closeness, betweenness,
                              local_clustering, triangles))
show(ralph_char_net_df)
####arrange rows by degree
ralph_char_net_df <- arrange(ralph_char_net_df, 
                             desc(degree),
                             desc(weightedDegree))
show(ralph_char_net_df)
####create table with gt
gt_ralph_char_net <- gt::gt(ralph_char_net_df, rownames_to_stub = TRUE)
show(gt_ralph_char_net)
####layout table
gt_ralph_char_net <-
  gt_ralph_char_net %>%
  #####add header
  tab_header(
    title = "Ralph Roister Doister",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.")
#####colour the table
gt_ralph_char_net <-
  gt_ralph_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Merrygreek")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Merrygreek"))
####show final layout
show(gt_ralph_char_net)
#####set margin to prevent cut off
gt_ralph_char_net <-
  gt_ralph_char_net %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_ralph_char_net)
####export table
gtsave(gt_ralph_char_net, "ralph_char_net.png", zoom = 10)

##[4.7.4.] evaluate the data regarding the hypotheses of the paper

###[4.7.4.1.] create a vector with the scheming characters 
ralph_selected_characters <- c("Merrygreek")
show(ralph_selected_characters)

###[4.7.4.2.] evaluate the count-based data

####[4.7.4.2.1.] check if the number of words, the number of speech acts, and 
####the number of scenes of the schemer are among the three best ranked
####values respectively
ralph_char_sp_df_eval_3 <-
  ralph_char_sp_df %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(ralph_char_sp_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
ralph_char_sp_df_eval_3_schemer <- 
  ralph_char_sp_df_eval_3 %>%
  filter(row.names(ralph_char_sp_df_eval_3) 
         %in% ralph_selected_characters)
show(ralph_char_sp_df_eval_3_schemer)

###[4.7.4.3.] evaluate the network data

####[4.7.4.3.1.] check if the degree centrality, weighted degree centrality,
####closeness centrality, betweenness centrality, and the number of triangles
####of the schemer are among the three best ranked values respectively.
####check if the clustering coefficient of the schemer
####is among the three lowest values.
ralph_char_net_df_eval_3 <-
  ralph_char_net_df %>%
  #####reverse values for local clustering
  mutate(local_clustering = -local_clustering) %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(ralph_char_net_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
ralph_char_net_df_eval_3_schemer <- 
  ralph_char_net_df_eval_3 %>%
  filter(row.names(ralph_char_net_df_eval_3) 
         %in% ralph_selected_characters)
show(ralph_char_net_df_eval_3_schemer)

####[4.7.4.3.2.] check if the degree centrality of the schemer is above
####average
ralph_char_net_df_eval_dc_av <-
  ralph_char_net_df %>%
  #####check if degree centrality is above average and extract column
  transmute(ralph_char_net_df$degree > ralph$averageDegree)
show(ralph_char_net_df_eval_dc_av)
#####rename column
ralph_char_net_df_eval_dc_av <-
  ralph_char_net_df_eval_dc_av %>%
  rename("d.c. > av. d.c." = 
           "ralph_char_net_df$degree > ralph$averageDegree")
show(ralph_char_net_df_eval_dc_av)
#####filter the selected character via logical vector to choose the schemer
ralph_char_net_df_eval_dc_av_schemer <-
  ralph_char_net_df_eval_dc_av %>%
  filter(row.names(ralph_char_net_df_eval_dc_av) %in% ralph_selected_characters)
show(ralph_char_net_df_eval_dc_av_schemer)

####[4.7.4.3.3.] check if the clustering coefficient of the schemer is below
####average
ralph_char_net_df_eval_cc_av <-
  ralph_char_net_df %>%
  #####check if clustering coefficient is below average and extract column
  transmute(ralph_char_net_df$local_clustering < ralph$averageClustering)
show(ralph_char_net_df_eval_cc_av)
#####rename column
ralph_char_net_df_eval_cc_av <-
  ralph_char_net_df_eval_cc_av %>%
  rename("c.c. < av. c.c." 
         = "ralph_char_net_df$local_clustering < ralph$averageClustering")
show(ralph_char_net_df_eval_cc_av)
#####filter the selected character via logical vector to choose the schemer
ralph_char_net_df_eval_cc_av_schemer <-
  ralph_char_net_df_eval_cc_av %>%
  filter(row.names(ralph_char_net_df_eval_cc_av) %in% ralph_selected_characters)
show(ralph_char_net_df_eval_cc_av_schemer)

##[4.7.5.] create network graph

###[4.7.5.1.] layout network graph
###choose layout algorithm for graph
ralph_coocur_layout <- create_layout(ralph_coocur, layout = "stress")
###layout settings
ralph_coocur_layout <-
  ggraph(ralph_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(ralph_coocur_layout)
###export graph
ggsave(ralph_coocur_layout,
       file = "ralph_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

#[5.] analyse the early modern French comedies (FreDraCor)

#[5.1.] Beaumarchais, Le barbier de Séville

##[5.1.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
barbier <- get_play_metadata(play = "beaumarchais-barbier-de-seville", 
                             corpus = "fre",
                             full_metadata = TRUE)
show(barbier)
####create matrix with general values for the dramatic network
barbier_gen_net_struc <- matrix(c(barbier$size, barbier$density,
                                  barbier$diameter, barbier$averageClustering,
                                  barbier$averagePathLength,
                                  barbier$averageDegree),
                                ncol = 1, byrow = FALSE)
###convert matrix to data frame
barbier_gen_net_struc_df <- as.data.frame(barbier_gen_net_struc)
###specify columns and rows for the data frame
colnames(barbier_gen_net_struc_df) <- c("value")
rownames(barbier_gen_net_struc_df) <- c("size", "density", "diameter",
                                        "average clustering",
                                        "average path length", "average degree")
show(barbier_gen_net_struc_df)
###create table with gt
gt_barbier_gen_net_struc <- gt::gt(barbier_gen_net_struc_df,
                                   rownames_to_stub = TRUE)
show(gt_barbier_gen_net_struc)
####amend table
gt_barbier_gen_net_struc <-
  gt_barbier_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Le barbier de Séville",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_barbier_gen_net_struc)
####export table
gtsave(gt_barbier_gen_net_struc, "barbier_gen_net_struc.png", zoom = 10)

##[5.1.2.] extract, calculate, and add character specific values
barbier_coocur <- get_net_cooccur_igraph(play =
                                           "beaumarchais-barbier-de-seville",
                                         corpus = "fre")
class(barbier_coocur)
###calculate local clustering
barbier_local_clustering <- transitivity(
  barbier_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
barbier_coocur <- set_vertex_attr(barbier_coocur, "local_clustering",
                                  value = barbier_local_clustering)
###calculate triangles
barbier_triangles <- count_triangles(barbier_coocur)
###add triangles
barbier_coocur <- set_vertex_attr(barbier_coocur, "triangles",
                                  value = barbier_triangles)
###show data
show(barbier_coocur)
class(barbier_coocur)
###export as data frame
barbier_char_spec_v_df <- as_data_frame(barbier_coocur, what="vertices")
show(barbier_char_spec_v_df)

##[5.1.3.] create two tables with character specific values

###[5.1.3.1.] create table with count-based measures for each character
####extract table with count-based measures for each character
barbier_char_sp_df <- select(barbier_char_spec_v_df,
                             c(numOfWords, numOfSpeechActs, numOfScenes))
show(barbier_char_sp_df)
####arrange rows by number of scenes
barbier_char_sp_df <- arrange(barbier_char_sp_df,
                              desc(numOfWords),
                              desc(numOfSpeechActs))
show(barbier_char_sp_df)
####create table with gt
gt_barbier_char_sp <- gt::gt(barbier_char_sp_df, rownames_to_stub = TRUE)
show(gt_barbier_char_sp)
####layout table
gt_barbier_char_sp <-
  gt_barbier_char_sp %>%
  #####add header
  tab_header(
    title = "Le barbier de Séville",
    subtitle = "count-based measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(numOfWords = "words",
             numOfSpeechActs = "speech acts",
             numOfScenes = "scenes") %>%
  #####bold row showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Figaro")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Figaro"))
#####colour the table: the higher the value, the darker the cell
gt_barbier_char_sp <-
  gt_barbier_char_sp %>%
  gt_color_rows(numOfWords:numOfScenes,
                palette = RColorBrewer::brewer.pal(5,"GnBu"))
#####show final layout
show(gt_barbier_char_sp)
####export table
gtsave(gt_barbier_char_sp, "barbier_char_sp.png", zoom = 10)

###[5.1.3.2.] create table with character specific network values
###extract table with character specific network values
barbier_char_net_df <- select(barbier_char_spec_v_df,
                              c(degree, weightedDegree,
                                closeness, betweenness,
                                local_clustering, triangles))
show(barbier_char_net_df)
####arrange rows by degree
barbier_char_net_df <- arrange(barbier_char_net_df, 
                               desc(degree),
                               desc(weightedDegree))
show(barbier_char_net_df)
####create table with gt
gt_barbier_char_net <- gt::gt(barbier_char_net_df, rownames_to_stub = TRUE)
show(gt_barbier_char_net)
####layout table
gt_barbier_char_net <-
  gt_barbier_char_net %>%
  #####add header
  tab_header(
    title = "Le barbier de Séville",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.")
#####colour the table
gt_barbier_char_net <-
  gt_barbier_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Figaro")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Figaro"))
####show final layout
show(gt_barbier_char_net)
####export table
gtsave(gt_barbier_char_net, "barbier_char_net.png", zoom = 10)

##[5.1.4.] evaluate the data regarding the hypotheses of the paper

###[5.1.4.1.] create a vector with the scheming characters 
barbier_selected_characters <- c("Figaro")
show(barbier_selected_characters)

###[5.1.4.2.] evaluate the count-based data

####[5.1.4.2.1.] check if the number of words, the number of speech acts, and 
####the number of scenes of the schemer are among the three best ranked
####values respectively
barbier_char_sp_df_eval_3 <-
  barbier_char_sp_df %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(barbier_char_sp_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
barbier_char_sp_df_eval_3_schemer <- 
  barbier_char_sp_df_eval_3 %>%
  filter(row.names(barbier_char_sp_df_eval_3) 
         %in% barbier_selected_characters)
show(barbier_char_sp_df_eval_3_schemer)

###[5.1.4.3.] evaluate the network data

####[5.1.4.3.1.] check if the degree centrality, weighted degree centrality,
####closeness centrality, betweenness centrality, and the number of triangles
####of the schemer are among the three best ranked values respectively.
####check if the clustering coefficient of the schemer
####is among the three lowest values.
barbier_char_net_df_eval_3 <-
  barbier_char_net_df %>%
  #####reverse values for local clustering
  mutate(local_clustering = -local_clustering) %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(barbier_char_net_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
barbier_char_net_df_eval_3_schemer <- 
  barbier_char_net_df_eval_3 %>%
  filter(row.names(barbier_char_net_df_eval_3) 
         %in% barbier_selected_characters)
show(barbier_char_net_df_eval_3_schemer)

####[5.1.4.3.2.] check if the degree centrality of the schemer is above
####average
barbier_char_net_df_eval_dc_av <-
  barbier_char_net_df %>%
  #####check if degree centrality is above average and extract column
  transmute(barbier_char_net_df$degree > barbier$averageDegree)
show(barbier_char_net_df_eval_dc_av)
#####rename column
barbier_char_net_df_eval_dc_av <-
  barbier_char_net_df_eval_dc_av %>%
  rename("d.c. > av. d.c." = 
           "barbier_char_net_df$degree > barbier$averageDegree")
show(barbier_char_net_df_eval_dc_av)
#####filter the selected character via logical vector to choose the schemer
barbier_char_net_df_eval_dc_av_schemer <-
  barbier_char_net_df_eval_dc_av %>%
  filter(row.names(barbier_char_net_df_eval_dc_av)
         %in% barbier_selected_characters)
show(barbier_char_net_df_eval_dc_av_schemer)

####[5.1.4.3.3.] check if the clustering coefficient of the schemer is below
####average
barbier_char_net_df_eval_cc_av <-
  barbier_char_net_df %>%
  #####check if clustering coefficient is below average and extract column
  transmute(barbier_char_net_df$local_clustering < barbier$averageClustering)
show(barbier_char_net_df_eval_cc_av)
#####rename column
barbier_char_net_df_eval_cc_av <-
  barbier_char_net_df_eval_cc_av %>%
  rename("c.c. < av. c.c." 
         = "barbier_char_net_df$local_clustering < barbier$averageClustering")
show(barbier_char_net_df_eval_cc_av)
#####filter the selected character via logical vector to choose the schemer
barbier_char_net_df_eval_cc_av_schemer <-
  barbier_char_net_df_eval_cc_av %>%
  filter(row.names(barbier_char_net_df_eval_cc_av)
         %in% barbier_selected_characters)
show(barbier_char_net_df_eval_cc_av_schemer)

##[5.1.5.] create network graph

###[5.1.5.1.] layout network graph
###choose layout algorithm for graph
barbier_coocur_layout <- create_layout(barbier_coocur, layout = "stress")
###layout settings
barbier_coocur_layout <-
  ggraph(barbier_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(barbier_coocur_layout)
###export graph
ggsave(barbier_coocur_layout,
       file = "barbier_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

#[5.2.] Cyrano, Le pédant joué

##[5.2.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
pedant <- get_play_metadata(play = "cyrano-pedant-joue", 
                            corpus = "fre",
                            full_metadata = TRUE)
show(pedant)
####create matrix with general values for the dramatic network
pedant_gen_net_struc <- matrix(c(pedant$size, pedant$density, pedant$diameter,
                                 pedant$averageClustering,
                                 pedant$averagePathLength,
                                 pedant$averageDegree),
                               ncol = 1, byrow = FALSE)
###convert matrix to data frame
pedant_gen_net_struc_df <- as.data.frame(pedant_gen_net_struc)
###specify columns and rows for the data frame
colnames(pedant_gen_net_struc_df) <- c("value")
rownames(pedant_gen_net_struc_df) <- c("size", "density", "diameter",
                                       "average clustering",
                                       "average path length", "average degree")
show(pedant_gen_net_struc_df)
###create table with gt
gt_pedant_gen_net_struc <- gt::gt(pedant_gen_net_struc_df,
                                  rownames_to_stub = TRUE)
show(gt_pedant_gen_net_struc)
####amend table
gt_pedant_gen_net_struc <-
  gt_pedant_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Le pédant joué",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_pedant_gen_net_struc)
####export table
gtsave(gt_pedant_gen_net_struc, "pedant_gen_net_struc.png", zoom = 10)

##[5.2.2.] extract, calculate, and add character specific values
pedant_coocur <- get_net_cooccur_igraph(play = "cyrano-pedant-joue", 
                                        corpus = "fre")
class(pedant_coocur)
###calculate local clustering
pedant_local_clustering <- transitivity(
  pedant_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
pedant_coocur <- set_vertex_attr(pedant_coocur, "local_clustering",
                                 value = pedant_local_clustering)
###calculate triangles
pedant_triangles <- count_triangles(pedant_coocur)
###add triangles
pedant_coocur <- set_vertex_attr(pedant_coocur, "triangles",
                                 value = pedant_triangles)
###show data
show(pedant_coocur)
class(pedant_coocur)
###export as data frame
pedant_char_spec_v_df <- as_data_frame(pedant_coocur, what="vertices")
show(pedant_char_spec_v_df)

##[5.2.3.] create two tables with character specific values

###[5.2.3.1.] create table with count-based measures for each character
####extract table with count-based measures for each character
pedant_char_sp_df <- select(pedant_char_spec_v_df,
                            c(numOfWords, numOfSpeechActs, numOfScenes))
show(pedant_char_sp_df)
####arrange rows by number of scenes
pedant_char_sp_df <- arrange(pedant_char_sp_df,
                             desc(numOfWords),
                             desc(numOfSpeechActs))
show(pedant_char_sp_df)
####create table with gt
gt_pedant_char_sp <- gt::gt(pedant_char_sp_df, rownames_to_stub = TRUE)
show(gt_pedant_char_sp)
####layout table
gt_pedant_char_sp <-
  gt_pedant_char_sp %>%
  #####add header
  tab_header(
    title = "Le pédant joué",
    subtitle = "count-based measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(numOfWords = "words",
             numOfSpeechActs = "speech acts",
             numOfScenes = "scenes") %>%
  #####bold row showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Corbineli")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Corbineli"))
#####colour the table: the higher the value, the darker the cell
gt_pedant_char_sp <-
  gt_pedant_char_sp %>%
  gt_color_rows(numOfWords:numOfScenes,
                palette = RColorBrewer::brewer.pal(5,"GnBu"))
#####show final layout
show(gt_pedant_char_sp)
####export table
gtsave(gt_pedant_char_sp, "pedant_char_sp.png", zoom = 10)

###[5.2.3.2.] create table with character specific network values
###extract table with character specific network values
pedant_char_net_df <- select(pedant_char_spec_v_df,
                             c(degree, weightedDegree,
                               closeness, betweenness,
                               local_clustering, triangles))
show(pedant_char_net_df)
####arrange rows by degree
pedant_char_net_df <- arrange(pedant_char_net_df,
                              desc(degree),
                              desc(weightedDegree))
show(pedant_char_net_df)
####create table with gt
gt_pedant_char_net <- gt::gt(pedant_char_net_df, rownames_to_stub = TRUE)
show(gt_pedant_char_net)
####layout table
gt_pedant_char_net <-
  gt_pedant_char_net %>%
  #####add header
  tab_header(
    title = "Le pédant joué",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.")
#####colour the table
gt_pedant_char_net <-
  gt_pedant_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Corbineli")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Corbineli"))
####show final layout
show(gt_pedant_char_net)
####export table
gtsave(gt_pedant_char_net, "pedant_char_net.png", zoom = 10)

##[5.2.4.] evaluate the data regarding the hypotheses of the paper

###[5.2.4.1.] create a vector with the scheming character 
pedant_selected_characters <- c("Corbineli")
show(pedant_selected_characters)

###[5.2.4.2.] evaluate the count-based data

####[5.2.4.2.1.] check if the number of words, the number of speech acts, and 
####the number of scenes of the schemer are among the three best ranked values
####respectively
pedant_char_sp_df_eval_3 <-
  pedant_char_sp_df %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(pedant_char_sp_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
pedant_char_sp_df_eval_3_schemer <- 
  pedant_char_sp_df_eval_3 %>%
  filter(row.names(pedant_char_sp_df_eval_3) %in% pedant_selected_characters)
show(pedant_char_sp_df_eval_3_schemer)

###[5.2.4.3.] evaluate the network data

####[5.2.4.3.1.] check if the degree centrality, weighted degree centrality,
####closeness centrality, betweenness centrality, and the number of triangles
####of the schemer are among the three best ranked values respectively.
####check if the clustering coefficient of the schemer is among the three 
####lowest values.
pedant_char_net_df_eval_3 <-
  pedant_char_net_df %>%
  #####reverse values for local clustering
  mutate(local_clustering = -local_clustering) %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(pedant_char_net_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
pedant_char_net_df_eval_3_schemer <- 
  pedant_char_net_df_eval_3 %>%
  filter(row.names(pedant_char_net_df_eval_3) %in% pedant_selected_characters)
show(pedant_char_net_df_eval_3_schemer)

####[5.2.4.3.2.] check if the degree centrality of the schemer is above average
pedant_char_net_df_eval_dc_av <-
  pedant_char_net_df %>%
  #####check if degree centrality is above average and extract column
  transmute(pedant_char_net_df$degree > pedant$averageDegree)
show(pedant_char_net_df_eval_dc_av)
####rename column
pedant_char_net_df_eval_dc_av <-
  pedant_char_net_df_eval_dc_av %>%
  rename("d.c. > av. d.c." = "pedant_char_net_df$degree > pedant$averageDegree")
show(pedant_char_net_df_eval_dc_av)
#####filter the selected character via logical vector to choose the schemer
pedant_char_net_df_eval_dc_av_schemer <-
  pedant_char_net_df_eval_dc_av %>%
  filter(row.names(pedant_char_net_df_eval_dc_av)
         %in% pedant_selected_characters)
show(pedant_char_net_df_eval_dc_av_schemer)

####[5.2.4.3.3.] check if the clustering coefficient of the schemer is below
####average
pedant_char_net_df_eval_cc_av <-
  pedant_char_net_df %>%
  #####check if clustering coefficient is below average and extract column
  transmute(pedant_char_net_df$local_clustering < pedant$averageClustering)
show(pedant_char_net_df_eval_cc_av)
#####rename column
pedant_char_net_df_eval_cc_av <-
  pedant_char_net_df_eval_cc_av %>%
  rename("c.c. < av. c.c." 
         = "pedant_char_net_df$local_clustering < pedant$averageClustering")
show(pedant_char_net_df_eval_cc_av)
#####filter the selected character via logical vector to choose the schemer
pedant_char_net_df_eval_cc_av_schemer <-
  pedant_char_net_df_eval_cc_av %>%
  filter(row.names(pedant_char_net_df_eval_cc_av)
         %in% pedant_selected_characters)
show(pedant_char_net_df_eval_cc_av_schemer)

##[5.2.5.] create network graph

###[5.2.5.2.] layout network graph
###choose layout algorithm for graph
pedant_coocur_layout <- create_layout(pedant_coocur, layout = "stress")
###layout settings
pedant_coocur_layout <-
  ggraph(pedant_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(pedant_coocur_layout)
###export graph
ggsave(pedant_coocur_layout,
       file = "pedant_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = TRUE,
       bg = "transparent")

#[5.3.] Mareschal, Le véritable Capitan Matamore

##[5.3.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
capitan <- get_play_metadata(play = "mareschala-veritable-capitaine-matamore", 
                             corpus = "fre",
                             full_metadata = TRUE)
show(capitan)
####create matrix with general values for the dramatic network
capitan_gen_net_struc <- matrix(c(capitan$size, capitan$density,
                                  capitan$diameter, capitan$averageClustering,
                                  capitan$averagePathLength,
                                  capitan$averageDegree),
                                ncol = 1, byrow = FALSE)
###convert matrix to data frame
capitan_gen_net_struc_df <- as.data.frame(capitan_gen_net_struc)
###specify columns and rows for the data frame
colnames(capitan_gen_net_struc_df) <- c("value")
rownames(capitan_gen_net_struc_df) <- c("size", "density", "diameter",
                                        "average clustering",
                                        "average path length", "average degree")
show(capitan_gen_net_struc_df)
###create table with gt
gt_capitan_gen_net_struc <- gt::gt(capitan_gen_net_struc_df,
                                   rownames_to_stub = TRUE)
show(gt_capitan_gen_net_struc)
####amend table
gt_capitan_gen_net_struc <-
  gt_capitan_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Le véritable Capitan Matamore",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_capitan_gen_net_struc)
####export table
gtsave(gt_capitan_gen_net_struc, "capitan_gen_net_struc.png", zoom = 10)

##[5.3.2.] extract, calculate, and add character specific values
capitan_coocur <-
  get_net_cooccur_igraph(play = "mareschala-veritable-capitaine-matamore",
                         corpus = "fre")
class(capitan_coocur)
###calculate local clustering
capitan_local_clustering <- transitivity(
  capitan_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
capitan_coocur <- set_vertex_attr(capitan_coocur, "local_clustering",
                                  value = capitan_local_clustering)
###calculate triangles
capitan_triangles <- count_triangles(capitan_coocur)
###add triangles
capitan_coocur <- set_vertex_attr(capitan_coocur, "triangles",
                                  value = capitan_triangles)
###show data
show(capitan_coocur)
class(capitan_coocur)
###export as data frame
capitan_char_spec_v_df <- as_data_frame(capitan_coocur, what="vertices")
show(capitan_char_spec_v_df)

##[5.3.3.] create two tables with character specific values

###[5.3.3.1.] create table with count-based measures for each character
####extract table with count-based measures for each character
capitan_char_sp_df <- select(capitan_char_spec_v_df,
                             c(numOfWords, numOfSpeechActs, numOfScenes))
show(capitan_char_sp_df)
####arrange rows by number of scenes
capitan_char_sp_df <- arrange(capitan_char_sp_df,
                              desc(numOfWords),
                              desc(numOfSpeechActs))
show(capitan_char_sp_df)
####create table with gt
gt_capitan_char_sp <- gt::gt(capitan_char_sp_df, rownames_to_stub = TRUE)
show(gt_capitan_char_sp)
####layout table
gt_capitan_char_sp <-
  gt_capitan_char_sp %>%
  #####add header
  tab_header(
    title = "Le véritable Capitan Matamore",
    subtitle = "count-based measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(numOfWords = "words",
             numOfSpeechActs = "speech acts",
             numOfScenes = "scenes") %>%
  #####bold rows showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Palestrion")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Palestrion"))
#####colour the table: the higher the value, the darker the cell
gt_capitan_char_sp <-
  gt_capitan_char_sp %>%
  gt_color_rows(numOfWords:numOfScenes,
                palette = RColorBrewer::brewer.pal(5,"GnBu"))
#####show final layout
show(gt_capitan_char_sp)
####export table
gtsave(gt_capitan_char_sp, "capitan_char_sp.png", zoom = 10)

###[5.3.3.2.] create table with character specific network values
###extract table with character specific network values
capitan_char_net_df <- select(capitan_char_spec_v_df,
                              c(degree, weightedDegree,
                                closeness, betweenness,
                                local_clustering, triangles))
show(capitan_char_net_df)
####arrange rows by degree
capitan_char_net_df <- arrange(capitan_char_net_df, 
                               desc(degree),
                               desc(weightedDegree))
show(capitan_char_net_df)
####create table with gt
gt_capitan_char_net <- gt::gt(capitan_char_net_df, rownames_to_stub = TRUE)
show(gt_capitan_char_net)
####layout table
gt_capitan_char_net <-
  gt_capitan_char_net %>%
  #####add header
  tab_header(
    title = "Le véritable Capitan Matamore",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.")
#####colour the table
gt_capitan_char_net <-
  gt_capitan_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold rows showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Palestrion")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Palestrion"))
####show final layout
show(gt_capitan_char_net)
####export table
gtsave(gt_capitan_char_net, "capitan_char_net.png", zoom = 10)

##[5.3.4.] evaluate the data regarding the hypotheses of the paper

###[5.3.4.1.] create a vector with the scheming characters 
capitan_selected_characters <- c("Palestrion")
show(capitan_selected_characters)

###[5.3.4.2.] evaluate the count-based data

####[5.3.4.2.1.] check if the number of words, the number of speech acts, and 
####the number of scenes of the schemer are among the three best ranked
####values respectively
capitan_char_sp_df_eval_3 <-
  capitan_char_sp_df %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(capitan_char_sp_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
capitan_char_sp_df_eval_3_schemer <- 
  capitan_char_sp_df_eval_3 %>%
  filter(row.names(capitan_char_sp_df_eval_3) 
         %in% capitan_selected_characters)
show(capitan_char_sp_df_eval_3_schemer)

###[5.3.4.3.] evaluate the network data

####[5.3.4.3.1.] check if the degree centrality, weighted degree centrality,
####closeness centrality, betweenness centrality, and the number of triangles
####of the schemer are among the three best ranked values respectively.
####check if the clustering coefficient of the schemer
####is among the three lowest values.
capitan_char_net_df_eval_3 <-
  capitan_char_net_df %>%
  #####reverse values for local clustering
  mutate(local_clustering = -local_clustering) %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(capitan_char_net_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
capitan_char_net_df_eval_3_schemer <- 
  capitan_char_net_df_eval_3 %>%
  filter(row.names(capitan_char_net_df_eval_3) 
         %in% capitan_selected_characters)
show(capitan_char_net_df_eval_3_schemer)

####[5.3.4.3.2.] check if the degree centrality of the schemer is above
####average
capitan_char_net_df_eval_dc_av <-
  capitan_char_net_df %>%
  #####check if degree centrality is above average and extract column
  transmute(capitan_char_net_df$degree > capitan$averageDegree)
show(capitan_char_net_df_eval_dc_av)
#####rename column
capitan_char_net_df_eval_dc_av <-
  capitan_char_net_df_eval_dc_av %>%
  rename("d.c. > av. d.c." = 
           "capitan_char_net_df$degree > capitan$averageDegree")
show(capitan_char_net_df_eval_dc_av)
#####filter the selected character via logical vector to choose the schemer
capitan_char_net_df_eval_dc_av_schemer <-
  capitan_char_net_df_eval_dc_av %>%
  filter(row.names(capitan_char_net_df_eval_dc_av)
         %in% capitan_selected_characters)
show(capitan_char_net_df_eval_dc_av_schemer)

####[5.3.4.3.3.] check if the clustering coefficient of the schemer is below
####average
capitan_char_net_df_eval_cc_av <-
  capitan_char_net_df %>%
  #####check if clustering coefficient is below average and extract column
  transmute(capitan_char_net_df$local_clustering < capitan$averageClustering)
show(capitan_char_net_df_eval_cc_av)
#####rename column
capitan_char_net_df_eval_cc_av <-
  capitan_char_net_df_eval_cc_av %>%
  rename("c.c. < av. c.c." 
         = "capitan_char_net_df$local_clustering < capitan$averageClustering")
show(capitan_char_net_df_eval_cc_av)
#####filter the selected character via logical vector to choose the schemer
capitan_char_net_df_eval_cc_av_schemer <-
  capitan_char_net_df_eval_cc_av %>%
  filter(row.names(capitan_char_net_df_eval_cc_av)
         %in% capitan_selected_characters)
show(capitan_char_net_df_eval_cc_av_schemer)

##[5.3.5.] create network graph

###[5.3.5.1.] layout network graph
###choose layout algorithm for graph
capitan_coocur_layout <- create_layout(capitan_coocur, layout = "stress")
###layout settings
capitan_coocur_layout <-
  ggraph(capitan_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(capitan_coocur_layout)
###export graph
ggsave(capitan_coocur_layout,
       file = "capitan_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

#[5.4.] Molière, L’étourdi ou Les contretemps

##[5.4.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
etourdi <- get_play_metadata(play = "moliere-etourdi", 
                             corpus = "fre",
                             full_metadata = TRUE)
show(etourdi)
####create matrix with general values for the dramatic network
etourdi_gen_net_struc <- matrix(c(etourdi$size, etourdi$density,
                                  etourdi$diameter, etourdi$averageClustering,
                                  etourdi$averagePathLength,
                                  etourdi$averageDegree),
                                ncol = 1, byrow = FALSE)
###convert matrix to data frame
etourdi_gen_net_struc_df <- as.data.frame(etourdi_gen_net_struc)
###specify columns and rows for the data frame
colnames(etourdi_gen_net_struc_df) <- c("value")
rownames(etourdi_gen_net_struc_df) <- c("size", "density", "diameter",
                                        "average clustering",
                                        "average path length", "average degree")
show(etourdi_gen_net_struc_df)
###create table with gt
gt_etourdi_gen_net_struc <- gt::gt(etourdi_gen_net_struc_df,
                                   rownames_to_stub = TRUE)
show(gt_etourdi_gen_net_struc)
####amend table
gt_etourdi_gen_net_struc <-
  gt_etourdi_gen_net_struc %>%
  #####add header
  tab_header(
    title = "L’étourdi",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_etourdi_gen_net_struc)
####export table
gtsave(gt_etourdi_gen_net_struc, "etourdi_gen_net_struc.png", zoom = 10)

##[5.4.2.] extract, calculate, and add character specific values
etourdi_coocur <- get_net_cooccur_igraph(play =
                                           "moliere-etourdi",
                                         corpus = "fre")
class(etourdi_coocur)
###calculate local clustering
etourdi_local_clustering <- transitivity(
  etourdi_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
etourdi_coocur <- set_vertex_attr(etourdi_coocur, "local_clustering",
                                  value = etourdi_local_clustering)
###calculate triangles
etourdi_triangles <- count_triangles(etourdi_coocur)
###add triangles
etourdi_coocur <- set_vertex_attr(etourdi_coocur, "triangles",
                                  value = etourdi_triangles)
###show data
show(etourdi_coocur)
class(etourdi_coocur)
###export as data frame
etourdi_char_spec_v_df <- as_data_frame(etourdi_coocur, what="vertices")
show(etourdi_char_spec_v_df)

##[5.4.3.] create two tables with character specific values

###[5.4.3.1.] create table with count-based measures for each character
####extract table with count-based measures for each character
etourdi_char_sp_df <- select(etourdi_char_spec_v_df,
                             c(numOfWords, numOfSpeechActs, numOfScenes))
show(etourdi_char_sp_df)
####arrange rows by number of scenes
etourdi_char_sp_df <- arrange(etourdi_char_sp_df,
                              desc(numOfWords),
                              desc(numOfSpeechActs))
show(etourdi_char_sp_df)
####create table with gt
gt_etourdi_char_sp <- gt::gt(etourdi_char_sp_df, rownames_to_stub = TRUE)
show(gt_etourdi_char_sp)
####layout table
gt_etourdi_char_sp <-
  gt_etourdi_char_sp %>%
  #####add header
  tab_header(
    title = "L’étourdi",
    subtitle = "count-based measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(numOfWords = "words",
             numOfSpeechActs = "speech acts",
             numOfScenes = "scenes") %>%
  #####bold rows showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Mascarille")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Mascarille"))
#####colour the table: the higher the value, the darker the cell
gt_etourdi_char_sp <-
  gt_etourdi_char_sp %>%
  gt_color_rows(numOfWords:numOfScenes,
                palette = RColorBrewer::brewer.pal(5,"GnBu"))
#####show final layout
show(gt_etourdi_char_sp)
####export table
gtsave(gt_etourdi_char_sp, "etourdi_char_sp.png", zoom = 10)

###[5.4.3.2.] create table with character specific network values
###extract table with character specific network values
etourdi_char_net_df <- select(etourdi_char_spec_v_df,
                              c(degree, weightedDegree,
                                closeness, betweenness,
                                local_clustering, triangles))
show(etourdi_char_net_df)
####arrange rows by degree
etourdi_char_net_df <- arrange(etourdi_char_net_df, 
                               desc(degree),
                               desc(weightedDegree))
show(etourdi_char_net_df)
####create table with gt
gt_etourdi_char_net <- gt::gt(etourdi_char_net_df, rownames_to_stub = TRUE)
show(gt_etourdi_char_net)
####layout table
gt_etourdi_char_net <-
  gt_etourdi_char_net %>%
  #####add header
  tab_header(
    title = "L’étourdi",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.")
#####colour the table
gt_etourdi_char_net <-
  gt_etourdi_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold rows showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Mascarille")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Mascarille"))
####show final layout
show(gt_etourdi_char_net)
####export table
gtsave(gt_etourdi_char_net, "etourdi_char_net.png", zoom = 10)

##[5.4.4.] evaluate the data regarding the hypotheses of the paper

###[5.4.4.1.] create a vector with the scheming characters 
etourdi_selected_characters <- c("Mascarille")
show(etourdi_selected_characters)

###[5.4.4.2.] evaluate the count-based data

####[5.4.4.2.1.] check if the number of words, the number of speech acts, and 
####the number of scenes of the schemer are among the three best ranked
####values respectively
etourdi_char_sp_df_eval_3 <-
  etourdi_char_sp_df %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(etourdi_char_sp_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
etourdi_char_sp_df_eval_3_schemer <- 
  etourdi_char_sp_df_eval_3 %>%
  filter(row.names(etourdi_char_sp_df_eval_3) 
         %in% etourdi_selected_characters)
show(etourdi_char_sp_df_eval_3_schemer)

###[5.4.4.3.] evaluate the network data

####[5.4.4.3.1.] check if the degree centrality, weighted degree centrality,
####closeness centrality, betweenness centrality, and the number of triangles
####of the schemer are among the three best ranked values respectively.
####check if the clustering coefficient of the schemer
####is among the three lowest values.
etourdi_char_net_df_eval_3 <-
  etourdi_char_net_df %>%
  #####reverse values for local clustering
  mutate(local_clustering = -local_clustering) %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(etourdi_char_net_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
etourdi_char_net_df_eval_3_schemer <- 
  etourdi_char_net_df_eval_3 %>%
  filter(row.names(etourdi_char_net_df_eval_3) 
         %in% etourdi_selected_characters)
show(etourdi_char_net_df_eval_3_schemer)

####[5.4.4.3.2.] check if the degree centrality of the schemer is above
####average
etourdi_char_net_df_eval_dc_av <-
  etourdi_char_net_df %>%
  #####check if degree centrality is above average and extract column
  transmute(etourdi_char_net_df$degree > etourdi$averageDegree)
show(etourdi_char_net_df_eval_dc_av)
#####rename column
etourdi_char_net_df_eval_dc_av <-
  etourdi_char_net_df_eval_dc_av %>%
  rename("d.c. > av. d.c." = 
           "etourdi_char_net_df$degree > etourdi$averageDegree")
show(etourdi_char_net_df_eval_dc_av)
#####filter the selected character via logical vector to choose the schemer
etourdi_char_net_df_eval_dc_av_schemer <-
  etourdi_char_net_df_eval_dc_av %>%
  filter(row.names(etourdi_char_net_df_eval_dc_av)
         %in% etourdi_selected_characters)
show(etourdi_char_net_df_eval_dc_av_schemer)

####[5.4.4.3.3.] check if the clustering coefficient of the schemer is below
####average
etourdi_char_net_df_eval_cc_av <-
  etourdi_char_net_df %>%
  #####check if clustering coefficient is below average and extract column
  transmute(etourdi_char_net_df$local_clustering < etourdi$averageClustering)
show(etourdi_char_net_df_eval_cc_av)
#####rename column
etourdi_char_net_df_eval_cc_av <-
  etourdi_char_net_df_eval_cc_av %>%
  rename("c.c. < av. c.c." 
         = "etourdi_char_net_df$local_clustering < etourdi$averageClustering")
show(etourdi_char_net_df_eval_cc_av)
#####filter the selected character via logical vector to choose the schemer
etourdi_char_net_df_eval_cc_av_schemer <-
  etourdi_char_net_df_eval_cc_av %>%
  filter(row.names(etourdi_char_net_df_eval_cc_av)
         %in% etourdi_selected_characters)
show(etourdi_char_net_df_eval_cc_av_schemer)

##[5.4.5.] create network graph

###[5.4.5.1.] layout network graph
###choose layout algorithm for graph
etourdi_coocur_layout <- create_layout(etourdi_coocur, layout = "stress")
###layout settings
etourdi_coocur_layout <-
  ggraph(etourdi_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(etourdi_coocur_layout)
###export graph
ggsave(etourdi_coocur_layout,
       file = "etourdi_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

#[5.5.] Molière, Les fourberies de Scapin

##[5.5.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
scapin <- get_play_metadata(play = "moliere-fourberies-de-scapin", 
                            corpus = "fre",
                            full_metadata = TRUE)
show(scapin)
####create matrix with general values for the dramatic network
scapin_gen_net_struc <- matrix(c(scapin$size, scapin$density,
                                 scapin$diameter, scapin$averageClustering,
                                 scapin$averagePathLength,
                                 scapin$averageDegree),
                               ncol = 1, byrow = FALSE)
###convert matrix to data frame
scapin_gen_net_struc_df <- as.data.frame(scapin_gen_net_struc)
###specify columns and rows for the data frame
colnames(scapin_gen_net_struc_df) <- c("value")
rownames(scapin_gen_net_struc_df) <- c("size", "density", "diameter",
                                       "average clustering",
                                       "average path length", "average degree")
show(scapin_gen_net_struc_df)
###create table with gt
gt_scapin_gen_net_struc <- gt::gt(scapin_gen_net_struc_df,
                                  rownames_to_stub = TRUE)
show(gt_scapin_gen_net_struc)
####amend table
gt_scapin_gen_net_struc <-
  gt_scapin_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Les fourberies de Scapin",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_scapin_gen_net_struc)
####export table
gtsave(gt_scapin_gen_net_struc, "scapin_gen_net_struc.png", zoom = 10)

##[5.5.2.] extract, calculate, and add character specific values
scapin_coocur <- get_net_cooccur_igraph(play =
                                          "moliere-fourberies-de-scapin",
                                        corpus = "fre")
class(scapin_coocur)
###calculate local clustering
scapin_local_clustering <- transitivity(
  scapin_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
scapin_coocur <- set_vertex_attr(scapin_coocur, "local_clustering",
                                 value = scapin_local_clustering)
###calculate triangles
scapin_triangles <- count_triangles(scapin_coocur)
###add triangles
scapin_coocur <- set_vertex_attr(scapin_coocur, "triangles",
                                 value = scapin_triangles)
###show data
show(scapin_coocur)
class(scapin_coocur)
###export as data frame
scapin_char_spec_v_df <- as_data_frame(scapin_coocur, what="vertices")
show(scapin_char_spec_v_df)

##[5.5.3.] create two tables with character specific values

###[5.5.3.1.] create table with count-based measures for each character
####extract table with count-based measures for each character
scapin_char_sp_df <- select(scapin_char_spec_v_df,
                            c(numOfWords, numOfSpeechActs, numOfScenes))
show(scapin_char_sp_df)
####arrange rows by number of scenes
scapin_char_sp_df <- arrange(scapin_char_sp_df,
                             desc(numOfWords),
                             desc(numOfSpeechActs))
show(scapin_char_sp_df)
####create table with gt
gt_scapin_char_sp <- gt::gt(scapin_char_sp_df, rownames_to_stub = TRUE)
show(gt_scapin_char_sp)
####layout table
gt_scapin_char_sp <-
  gt_scapin_char_sp %>%
  #####add header
  tab_header(
    title = "Les fourberies de Scapin",
    subtitle = "count-based measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(numOfWords = "words",
             numOfSpeechActs = "speech acts",
             numOfScenes = "scenes") %>%
  #####bold rows showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Scapin")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Scapin"))
#####colour the table: the higher the value, the darker the cell
gt_scapin_char_sp <-
  gt_scapin_char_sp %>%
  gt_color_rows(numOfWords:numOfScenes,
                palette = RColorBrewer::brewer.pal(5,"GnBu"))
#####show final layout
show(gt_scapin_char_sp)
####export table
gtsave(gt_scapin_char_sp, "scapin_char_sp.png", zoom = 10)

###[5.5.3.2.] create table with character specific network values
###extract table with character specific network values
scapin_char_net_df <- select(scapin_char_spec_v_df,
                             c(degree, weightedDegree,
                               closeness, betweenness,
                               local_clustering, triangles))
show(scapin_char_net_df)
####arrange rows by degree
scapin_char_net_df <- arrange(scapin_char_net_df, 
                              desc(degree),
                              desc(weightedDegree))
show(scapin_char_net_df)
####create table with gt
gt_scapin_char_net <- gt::gt(scapin_char_net_df, rownames_to_stub = TRUE)
show(gt_scapin_char_net)
####layout table
gt_scapin_char_net <-
  gt_scapin_char_net %>%
  #####add header
  tab_header(
    title = "Les fourberies de Scapin",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.")
#####colour the table
gt_scapin_char_net <-
  gt_scapin_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold rows showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Scapin")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Scapin"))
####show final layout
show(gt_scapin_char_net)
####export table
gtsave(gt_scapin_char_net, "scapin_char_net.png", zoom = 10)

##[5.5.4.] evaluate the data regarding the hypotheses of the paper

###[5.5.4.1.] create a vector with the scheming characters 
scapin_selected_characters <- c("Scapin")
show(scapin_selected_characters)

###[5.5.4.2.] evaluate the count-based data

####[5.5.4.2.1.] check if the number of words, the number of speech acts, and 
####the number of scenes of the schemer are among the three best ranked
####values respectively
scapin_char_sp_df_eval_3 <-
  scapin_char_sp_df %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(scapin_char_sp_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
scapin_char_sp_df_eval_3_schemer <- 
  scapin_char_sp_df_eval_3 %>%
  filter(row.names(scapin_char_sp_df_eval_3) 
         %in% scapin_selected_characters)
show(scapin_char_sp_df_eval_3_schemer)

###[5.5.4.3.] evaluate the network data

####[5.5.4.3.1.] check if the degree centrality, weighted degree centrality,
####closeness centrality, betweenness centrality, and the number of triangles
####of the schemer are among the three best ranked values respectively.
####check if the clustering coefficient of the schemer
####is among the three lowest values.
scapin_char_net_df_eval_3 <-
  scapin_char_net_df %>%
  #####reverse values for local clustering
  mutate(local_clustering = -local_clustering) %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(scapin_char_net_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
scapin_char_net_df_eval_3_schemer <- 
  scapin_char_net_df_eval_3 %>%
  filter(row.names(scapin_char_net_df_eval_3) 
         %in% scapin_selected_characters)
show(scapin_char_net_df_eval_3_schemer)

####[5.5.4.3.2.] check if the degree centrality of the schemer is above
####average
scapin_char_net_df_eval_dc_av <-
  scapin_char_net_df %>%
  #####check if degree centrality is above average and extract column
  transmute(scapin_char_net_df$degree > scapin$averageDegree)
show(scapin_char_net_df_eval_dc_av)
#####rename column
scapin_char_net_df_eval_dc_av <-
  scapin_char_net_df_eval_dc_av %>%
  rename("d.c. > av. d.c." = 
           "scapin_char_net_df$degree > scapin$averageDegree")
show(scapin_char_net_df_eval_dc_av)
#####filter the selected character via logical vector to choose the schemer
scapin_char_net_df_eval_dc_av_schemer <-
  scapin_char_net_df_eval_dc_av %>%
  filter(row.names(scapin_char_net_df_eval_dc_av)
         %in% scapin_selected_characters)
show(scapin_char_net_df_eval_dc_av_schemer)

####[5.5.4.3.3.] check if the clustering coefficient of the schemer is below
####average
scapin_char_net_df_eval_cc_av <-
  scapin_char_net_df %>%
  #####check if clustering coefficient is below average and extract column
  transmute(scapin_char_net_df$local_clustering < scapin$averageClustering)
show(scapin_char_net_df_eval_cc_av)
#####rename column
scapin_char_net_df_eval_cc_av <-
  scapin_char_net_df_eval_cc_av %>%
  rename("c.c. < av. c.c." 
         = "scapin_char_net_df$local_clustering < scapin$averageClustering")
show(scapin_char_net_df_eval_cc_av)
#####filter the selected character via logical vector to choose the schemer
scapin_char_net_df_eval_cc_av_schemer <-
  scapin_char_net_df_eval_cc_av %>%
  filter(row.names(scapin_char_net_df_eval_cc_av)
         %in% scapin_selected_characters)
show(scapin_char_net_df_eval_cc_av_schemer)

##[5.5.5.] create network graph

###[5.5.5.1.] layout network graph
###choose layout algorithm for graph
scapin_coocur_layout <- create_layout(scapin_coocur, layout = "stress")
###layout settings
scapin_coocur_layout <-
  ggraph(scapin_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(scapin_coocur_layout)
###export graph
ggsave(scapin_coocur_layout,
       file = "scapin_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

#[5.6.] Regnard, Le retour imprévu

##[5.6.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
retour <- get_play_metadata(play = "regnard-retour-imprevu", 
                            corpus = "fre",
                            full_metadata = TRUE)
show(retour)
####create matrix with general values for the dramatic network
retour_gen_net_struc <- matrix(c(retour$size, retour$density,
                                 retour$diameter, retour$averageClustering,
                                 retour$averagePathLength,
                                 retour$averageDegree),
                               ncol = 1, byrow = FALSE)
###convert matrix to data frame
retour_gen_net_struc_df <- as.data.frame(retour_gen_net_struc)
###specify columns and rows for the data frame
colnames(retour_gen_net_struc_df) <- c("value")
rownames(retour_gen_net_struc_df) <- c("size", "density", "diameter",
                                       "average clustering",
                                       "average path length", "average degree")
show(retour_gen_net_struc_df)
###create table with gt
gt_retour_gen_net_struc <- gt::gt(retour_gen_net_struc_df,
                                  rownames_to_stub = TRUE)
show(gt_retour_gen_net_struc)
####amend table
gt_retour_gen_net_struc <-
  gt_retour_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Le retour imprévu",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_retour_gen_net_struc)
####export table
gtsave(gt_retour_gen_net_struc, "retour_gen_net_struc.png", zoom = 10)

##[5.6.2.] extract, calculate, and add character specific values
retour_coocur <- get_net_cooccur_igraph(play =
                                          "regnard-retour-imprevu",
                                        corpus = "fre")
class(retour_coocur)
###calculate local clustering
retour_local_clustering <- transitivity(
  retour_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
retour_coocur <- set_vertex_attr(retour_coocur, "local_clustering",
                                 value = retour_local_clustering)
###calculate triangles
retour_triangles <- count_triangles(retour_coocur)
###add triangles
retour_coocur <- set_vertex_attr(retour_coocur, "triangles",
                                 value = retour_triangles)
###show data
show(retour_coocur)
class(retour_coocur)
###export as data frame
retour_char_spec_v_df <- as_data_frame(retour_coocur, what="vertices")
show(retour_char_spec_v_df)

##[5.6.3.] create two tables with character specific values

###[5.6.3.1.] create table with count-based measures for each character
####extract table with count-based measures for each character
retour_char_sp_df <- select(retour_char_spec_v_df,
                            c(numOfWords, numOfSpeechActs, numOfScenes))
show(retour_char_sp_df)
####arrange rows by number of scenes
retour_char_sp_df <- arrange(retour_char_sp_df,
                             desc(numOfWords),
                             desc(numOfSpeechActs))
show(retour_char_sp_df)
####create table with gt
gt_retour_char_sp <- gt::gt(retour_char_sp_df, rownames_to_stub = TRUE)
show(gt_retour_char_sp)
####layout table
gt_retour_char_sp <-
  gt_retour_char_sp %>%
  #####add header
  tab_header(
    title = "Le retour imprévu",
    subtitle = "count-based measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(numOfWords = "words",
             numOfSpeechActs = "speech acts",
             numOfScenes = "scenes") %>%
  #####bold rows showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Merlin")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Merlin"))
#####colour the table: the higher the value, the darker the cell
gt_retour_char_sp <-
  gt_retour_char_sp %>%
  gt_color_rows(numOfWords:numOfScenes,
                palette = RColorBrewer::brewer.pal(5,"GnBu"))
#####show final layout
show(gt_retour_char_sp)
####export table
gtsave(gt_retour_char_sp, "retour_char_sp.png", zoom = 10)

###[5.6.3.2.] create table with character specific network values
###extract table with character specific network values
retour_char_net_df <- select(retour_char_spec_v_df,
                             c(degree, weightedDegree,
                               closeness, betweenness,
                               local_clustering, triangles))
show(retour_char_net_df)
####arrange rows by degree
retour_char_net_df <- arrange(retour_char_net_df, 
                              desc(degree),
                              desc(weightedDegree))
show(retour_char_net_df)
####create table with gt
gt_retour_char_net <- gt::gt(retour_char_net_df, rownames_to_stub = TRUE)
show(gt_retour_char_net)
####layout table
gt_retour_char_net <-
  gt_retour_char_net %>%
  #####add header
  tab_header(
    title = "Le retour imprévu",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.")
#####colour the table
gt_retour_char_net <-
  gt_retour_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold rows showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Merlin")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Merlin"))
####show final layout
show(gt_retour_char_net)
####export table
gtsave(gt_retour_char_net, "retour_char_net.png", zoom = 10)

##[5.6.4.] evaluate the data regarding the hypotheses of the paper

###[5.6.4.1.] create a vector with the scheming characters 
retour_selected_characters <- c("Merlin")
show(retour_selected_characters)

###[5.6.4.2.] evaluate the count-based data

####[5.6.4.2.1.] check if the number of words, the number of speech acts, and 
####the number of scenes of the schemer are among the three best ranked
####values respectively
retour_char_sp_df_eval_3 <-
  retour_char_sp_df %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(retour_char_sp_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
retour_char_sp_df_eval_3_schemer <- 
  retour_char_sp_df_eval_3 %>%
  filter(row.names(retour_char_sp_df_eval_3) 
         %in% retour_selected_characters)
show(retour_char_sp_df_eval_3_schemer)

###[5.6.4.3.] evaluate the network data

####[5.6.4.3.1.] check if the degree centrality, weighted degree centrality,
####closeness centrality, betweenness centrality, and the number of triangles
####of the schemer are among the three best ranked values respectively.
####check if the clustering coefficient of the schemer
####is among the three lowest values.
retour_char_net_df_eval_3 <-
  retour_char_net_df %>%
  #####reverse values for local clustering
  mutate(local_clustering = -local_clustering) %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(retour_char_net_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
retour_char_net_df_eval_3_schemer <- 
  retour_char_net_df_eval_3 %>%
  filter(row.names(retour_char_net_df_eval_3) 
         %in% retour_selected_characters)
show(retour_char_net_df_eval_3_schemer)

####[5.6.4.3.2.] check if the degree centrality of the schemer is above
####average
retour_char_net_df_eval_dc_av <-
  retour_char_net_df %>%
  #####check if degree centrality is above average and extract column
  transmute(retour_char_net_df$degree > retour$averageDegree)
show(retour_char_net_df_eval_dc_av)
#####rename column
retour_char_net_df_eval_dc_av <-
  retour_char_net_df_eval_dc_av %>%
  rename("d.c. > av. d.c." = 
           "retour_char_net_df$degree > retour$averageDegree")
show(retour_char_net_df_eval_dc_av)
#####filter the selected character via logical vector to choose the schemer
retour_char_net_df_eval_dc_av_schemer <-
  retour_char_net_df_eval_dc_av %>%
  filter(row.names(retour_char_net_df_eval_dc_av)
         %in% retour_selected_characters)
show(retour_char_net_df_eval_dc_av_schemer)

####[5.6.4.3.3.] check if the clustering coefficient of the schemer is below
####average
retour_char_net_df_eval_cc_av <-
  retour_char_net_df %>%
  #####check if clustering coefficient is below average and extract column
  transmute(retour_char_net_df$local_clustering < retour$averageClustering)
show(retour_char_net_df_eval_cc_av)
#####rename column
retour_char_net_df_eval_cc_av <-
  retour_char_net_df_eval_cc_av %>%
  rename("c.c. < av. c.c." 
         = "retour_char_net_df$local_clustering < retour$averageClustering")
show(retour_char_net_df_eval_cc_av)
#####filter the selected character via logical vector to choose the schemer
retour_char_net_df_eval_cc_av_schemer <-
  retour_char_net_df_eval_cc_av %>%
  filter(row.names(retour_char_net_df_eval_cc_av)
         %in% retour_selected_characters)
show(retour_char_net_df_eval_cc_av_schemer)

##[5.6.5.] create network graph

###[5.6.5.1.] layout network graph
###choose layout algorithm for graph
retour_coocur_layout <- create_layout(retour_coocur, layout = "stress")
###layout settings
retour_coocur_layout <-
  ggraph(retour_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(retour_coocur_layout)
###export graph
ggsave(retour_coocur_layout,
       file = "retour_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

#[5.7.] Regnard, La sérénade

##[5.7.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
serenade <- get_play_metadata(play = "regnard-serenade", 
                              corpus = "fre",
                              full_metadata = TRUE)
show(serenade)
####create matrix with general values for the dramatic network
serenade_gen_net_struc <- matrix(c(serenade$size, serenade$density,
                                   serenade$diameter,
                                   serenade$averageClustering,
                                   serenade$averagePathLength,
                                   serenade$averageDegree),
                                 ncol = 1, byrow = FALSE)
###convert matrix to data frame
serenade_gen_net_struc_df <- as.data.frame(serenade_gen_net_struc)
###specify columns and rows for the data frame
colnames(serenade_gen_net_struc_df) <- c("value")
rownames(serenade_gen_net_struc_df) <- c("size", "density", "diameter",
                                         "average clustering",
                                         "average path length",
                                         "average degree")
show(serenade_gen_net_struc_df)
###create table with gt
gt_serenade_gen_net_struc <- gt::gt(serenade_gen_net_struc_df,
                                    rownames_to_stub = TRUE)
show(gt_serenade_gen_net_struc)
####amend table
gt_serenade_gen_net_struc <-
  gt_serenade_gen_net_struc %>%
  #####add header
  tab_header(
    title = "La sérénade",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_serenade_gen_net_struc)
####export table
gtsave(gt_serenade_gen_net_struc, "serenade_gen_net_struc.png", zoom = 10)

##[5.7.2.] extract, calculate, and add character specific values
serenade_coocur <- get_net_cooccur_igraph(play =
                                            "regnard-serenade",
                                          corpus = "fre")
class(serenade_coocur)
###calculate local clustering
serenade_local_clustering <- transitivity(
  serenade_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
serenade_coocur <- set_vertex_attr(serenade_coocur, "local_clustering",
                                   value = serenade_local_clustering)
###calculate triangles
serenade_triangles <- count_triangles(serenade_coocur)
###add triangles
serenade_coocur <- set_vertex_attr(serenade_coocur, "triangles",
                                   value = serenade_triangles)
###show data
show(serenade_coocur)
class(serenade_coocur)
###export as data frame
serenade_char_spec_v_df <- as_data_frame(serenade_coocur, what="vertices")
show(serenade_char_spec_v_df)

##[5.7.3.] create two tables with character specific values

###[5.7.3.1.] create table with count-based measures for each character
####extract table with count-based measures for each character
serenade_char_sp_df <- select(serenade_char_spec_v_df,
                              c(numOfWords, numOfSpeechActs, numOfScenes))
show(serenade_char_sp_df)
####arrange rows by number of scenes
serenade_char_sp_df <- arrange(serenade_char_sp_df,
                               desc(numOfWords),
                               desc(numOfSpeechActs))
show(serenade_char_sp_df)
####create table with gt
gt_serenade_char_sp <- gt::gt(serenade_char_sp_df, rownames_to_stub = TRUE)
show(gt_serenade_char_sp)
####layout table
gt_serenade_char_sp <-
  gt_serenade_char_sp %>%
  #####add header
  tab_header(
    title = "La sérénade",
    subtitle = "count-based measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(numOfWords = "words",
             numOfSpeechActs = "speech acts",
             numOfScenes = "scenes") %>%
  #####bold rows showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Scapin")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Scapin"))
#####colour the table: the higher the value, the darker the cell
gt_serenade_char_sp <-
  gt_serenade_char_sp %>%
  gt_color_rows(numOfWords:numOfScenes,
                palette = RColorBrewer::brewer.pal(5,"GnBu"))
#####show final layout
show(gt_serenade_char_sp)
####export table
gtsave(gt_serenade_char_sp, "serenade_char_sp.png", zoom = 10)

###[5.7.3.2.] create table with character specific network values
###extract table with character specific network values
serenade_char_net_df <- select(serenade_char_spec_v_df,
                               c(degree, weightedDegree,
                                 closeness, betweenness,
                                 local_clustering, triangles))
show(serenade_char_net_df)
####arrange rows by degree
serenade_char_net_df <- arrange(serenade_char_net_df, 
                                desc(degree),
                                desc(weightedDegree))
show(serenade_char_net_df)
####create table with gt
gt_serenade_char_net <- gt::gt(serenade_char_net_df, rownames_to_stub = TRUE)
show(gt_serenade_char_net)
####layout table
gt_serenade_char_net <-
  gt_serenade_char_net %>%
  #####add header
  tab_header(
    title = "La sérénade",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.")
#####colour the table
gt_serenade_char_net <-
  gt_serenade_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold rows showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Scapin")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Scapin"))
####show final layout
show(gt_serenade_char_net)
####export table
gtsave(gt_serenade_char_net, "serenade_char_net.png", zoom = 10)

##[5.7.4.] evaluate the data regarding the hypotheses of the paper

###[5.7.4.1.] create a vector with the scheming characters 
serenade_selected_characters <- c("Scapin")
show(serenade_selected_characters)

###[5.7.4.2.] evaluate the count-based data

####[5.7.4.2.1.] check if the number of words, the number of speech acts, and 
####the number of scenes of the schemer are among the three best ranked
####values respectively
serenade_char_sp_df_eval_3 <-
  serenade_char_sp_df %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(serenade_char_sp_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
serenade_char_sp_df_eval_3_schemer <- 
  serenade_char_sp_df_eval_3 %>%
  filter(row.names(serenade_char_sp_df_eval_3) 
         %in% serenade_selected_characters)
show(serenade_char_sp_df_eval_3_schemer)

###[5.7.4.3.] evaluate the network data

####[5.7.4.3.1.] check if the degree centrality, weighted degree centrality,
####closeness centrality, betweenness centrality, and the number of triangles
####of the schemer are among the three best ranked values respectively.
####check if the clustering coefficient of the schemer
####is among the three lowest values.
serenade_char_net_df_eval_3 <-
  serenade_char_net_df %>%
  #####reverse values for local clustering
  mutate(local_clustering = -local_clustering) %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(serenade_char_net_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
serenade_char_net_df_eval_3_schemer <- 
  serenade_char_net_df_eval_3 %>%
  filter(row.names(serenade_char_net_df_eval_3) 
         %in% serenade_selected_characters)
show(serenade_char_net_df_eval_3_schemer)

####[5.7.4.3.2.] check if the degree centrality of the schemer is above
####average
serenade_char_net_df_eval_dc_av <-
  serenade_char_net_df %>%
  #####check if degree centrality is above average and extract column
  transmute(serenade_char_net_df$degree > serenade$averageDegree)
show(serenade_char_net_df_eval_dc_av)
#####rename column
serenade_char_net_df_eval_dc_av <-
  serenade_char_net_df_eval_dc_av %>%
  rename("d.c. > av. d.c." = 
           "serenade_char_net_df$degree > serenade$averageDegree")
show(serenade_char_net_df_eval_dc_av)
#####filter the selected character via logical vector to choose the schemer
serenade_char_net_df_eval_dc_av_schemer <-
  serenade_char_net_df_eval_dc_av %>%
  filter(row.names(serenade_char_net_df_eval_dc_av)
         %in% serenade_selected_characters)
show(serenade_char_net_df_eval_dc_av_schemer)

####[5.7.4.3.3.] check if the clustering coefficient of the schemer is below
####average
serenade_char_net_df_eval_cc_av <-
  serenade_char_net_df %>%
  #####check if clustering coefficient is below average and extract column
  transmute(serenade_char_net_df$local_clustering < serenade$averageClustering)
show(serenade_char_net_df_eval_cc_av)
#####rename column
serenade_char_net_df_eval_cc_av <-
  serenade_char_net_df_eval_cc_av %>%
  rename("c.c. < av. c.c." 
         = "serenade_char_net_df$local_clustering < serenade$averageClustering")
show(serenade_char_net_df_eval_cc_av)
#####filter the selected character via logical vector to choose the schemer
serenade_char_net_df_eval_cc_av_schemer <-
  serenade_char_net_df_eval_cc_av %>%
  filter(row.names(serenade_char_net_df_eval_cc_av)
         %in% serenade_selected_characters)
show(serenade_char_net_df_eval_cc_av_schemer)

##[5.7.5.] create network graph

###[5.7.5.1.] layout network graph
###choose layout algorithm for graph
serenade_coocur_layout <- create_layout(serenade_coocur, layout = "stress")
###layout settings
serenade_coocur_layout <-
  ggraph(serenade_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(serenade_coocur_layout)
###export graph
ggsave(serenade_coocur_layout,
       file = "serenade_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

#[5.8.] Rotrou, Les captifs

##[5.8.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
captifs <- get_play_metadata(play = "rotrou-captifs", 
                             corpus = "fre",
                             full_metadata = TRUE)
show(captifs)
####create matrix with general values for the dramatic network
captifs_gen_net_struc <- matrix(c(captifs$size, captifs$density,
                                  captifs$diameter, captifs$averageClustering,
                                  captifs$averagePathLength,
                                  captifs$averageDegree),
                                ncol = 1, byrow = FALSE)
###convert matrix to data frame
captifs_gen_net_struc_df <- as.data.frame(captifs_gen_net_struc)
###specify columns and rows for the data frame
colnames(captifs_gen_net_struc_df) <- c("value")
rownames(captifs_gen_net_struc_df) <- c("size", "density", "diameter",
                                        "average clustering",
                                        "average path length", "average degree")
show(captifs_gen_net_struc_df)
###create table with gt
gt_captifs_gen_net_struc <- gt::gt(captifs_gen_net_struc_df,
                                   rownames_to_stub = TRUE)
show(gt_captifs_gen_net_struc)
####amend table
gt_captifs_gen_net_struc <-
  gt_captifs_gen_net_struc %>%
  #####add header
  tab_header(
    title = "Les captifs",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_captifs_gen_net_struc)
####export table
gtsave(gt_captifs_gen_net_struc, "captifs_gen_net_struc.png", zoom = 10)

##[5.8.2.] extract, calculate, and add character specific values
captifs_coocur <- get_net_cooccur_igraph(play =
                                           "rotrou-captifs",
                                         corpus = "fre")
class(captifs_coocur)
###calculate local clustering
captifs_local_clustering <- transitivity(
  captifs_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
captifs_coocur <- set_vertex_attr(captifs_coocur, "local_clustering",
                                  value = captifs_local_clustering)
###calculate triangles
captifs_triangles <- count_triangles(captifs_coocur)
###add triangles
captifs_coocur <- set_vertex_attr(captifs_coocur, "triangles",
                                  value = captifs_triangles)
###show data
show(captifs_coocur)
class(captifs_coocur)
###export as data frame
captifs_char_spec_v_df <- as_data_frame(captifs_coocur, what="vertices")
show(captifs_char_spec_v_df)

##[5.8.3.] create two tables with character specific values

###[5.8.3.1.] create table with count-based measures for each character
####extract table with count-based measures for each character
captifs_char_sp_df <- select(captifs_char_spec_v_df,
                             c(numOfWords, numOfSpeechActs, numOfScenes))
show(captifs_char_sp_df)
####arrange rows by number of scenes
captifs_char_sp_df <- arrange(captifs_char_sp_df,
                              desc(numOfWords),
                              desc(numOfSpeechActs))
show(captifs_char_sp_df)
####create table with gt
gt_captifs_char_sp <- gt::gt(captifs_char_sp_df, rownames_to_stub = TRUE)
show(gt_captifs_char_sp)
####layout table
gt_captifs_char_sp <-
  gt_captifs_char_sp %>%
  #####add header
  tab_header(
    title = "Les captifs",
    subtitle = "count-based measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(numOfWords = "words",
             numOfSpeechActs = "speech acts",
             numOfScenes = "scenes") %>%
  #####bold rows showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Tyndare")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Tyndare"))
#####colour the table: the higher the value, the darker the cell
gt_captifs_char_sp <-
  gt_captifs_char_sp %>%
  gt_color_rows(numOfWords:numOfScenes,
                palette = RColorBrewer::brewer.pal(5,"GnBu"))
#####show final layout
show(gt_captifs_char_sp)
####export table
gtsave(gt_captifs_char_sp, "captifs_char_sp.png", zoom = 10)

###[5.8.3.2.] create table with character specific network values
###extract table with character specific network values
captifs_char_net_df <- select(captifs_char_spec_v_df,
                              c(degree, weightedDegree,
                                closeness, betweenness,
                                local_clustering, triangles))
show(captifs_char_net_df)
####arrange rows by degree
captifs_char_net_df <- arrange(captifs_char_net_df, 
                               desc(degree),
                               desc(weightedDegree))
show(captifs_char_net_df)
####create table with gt
gt_captifs_char_net <- gt::gt(captifs_char_net_df, rownames_to_stub = TRUE)
show(gt_captifs_char_net)
####layout table
gt_captifs_char_net <-
  gt_captifs_char_net %>%
  #####add header
  tab_header(
    title = "Les captifs",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.")
#####colour the table
gt_captifs_char_net <-
  gt_captifs_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold rows showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Tyndare")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Tyndare"))
####show final layout
show(gt_captifs_char_net)
####export table
gtsave(gt_captifs_char_net, "captifs_char_net.png", zoom = 10)

##[5.8.4.] evaluate the data regarding the hypotheses of the paper

###[5.8.4.1.] create a vector with the scheming characters 
captifs_selected_characters <- c("Tyndare")
show(captifs_selected_characters)

###[5.8.4.2.] evaluate the count-based data

####[5.8.4.2.1.] check if the number of words, the number of speech acts, and 
####the number of scenes of the schemer are among the three best ranked
####values respectively
captifs_char_sp_df_eval_3 <-
  captifs_char_sp_df %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(captifs_char_sp_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
captifs_char_sp_df_eval_3_schemer <- 
  captifs_char_sp_df_eval_3 %>%
  filter(row.names(captifs_char_sp_df_eval_3) 
         %in% captifs_selected_characters)
show(captifs_char_sp_df_eval_3_schemer)

###[5.8.4.3.] evaluate the network data

####[5.8.4.3.1.] check if the degree centrality, weighted degree centrality,
####closeness centrality, betweenness centrality, and the number of triangles
####of the schemer are among the three best ranked values respectively.
####check if the clustering coefficient of the schemer
####is among the three lowest values.
captifs_char_net_df_eval_3 <-
  captifs_char_net_df %>%
  #####reverse values for local clustering
  mutate(local_clustering = -local_clustering) %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(captifs_char_net_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
captifs_char_net_df_eval_3_schemer <- 
  captifs_char_net_df_eval_3 %>%
  filter(row.names(captifs_char_net_df_eval_3) 
         %in% captifs_selected_characters)
show(captifs_char_net_df_eval_3_schemer)

####[5.8.4.3.2.] check if the degree centrality of the schemer is above
####average
captifs_char_net_df_eval_dc_av <-
  captifs_char_net_df %>%
  #####check if degree centrality is above average and extract column
  transmute(captifs_char_net_df$degree > captifs$averageDegree)
show(captifs_char_net_df_eval_dc_av)
#####rename column
captifs_char_net_df_eval_dc_av <-
  captifs_char_net_df_eval_dc_av %>%
  rename("d.c. > av. d.c." = 
           "captifs_char_net_df$degree > captifs$averageDegree")
show(captifs_char_net_df_eval_dc_av)
#####filter the selected character via logical vector to choose the schemer
captifs_char_net_df_eval_dc_av_schemer <-
  captifs_char_net_df_eval_dc_av %>%
  filter(row.names(captifs_char_net_df_eval_dc_av)
         %in% captifs_selected_characters)
show(captifs_char_net_df_eval_dc_av_schemer)

####[5.8.4.3.3.] check if the clustering coefficient of the schemer is below
####average
captifs_char_net_df_eval_cc_av <-
  captifs_char_net_df %>%
  #####check if clustering coefficient is below average and extract column
  transmute(captifs_char_net_df$local_clustering < captifs$averageClustering)
show(captifs_char_net_df_eval_cc_av)
#####rename column
captifs_char_net_df_eval_cc_av <-
  captifs_char_net_df_eval_cc_av %>%
  rename("c.c. < av. c.c." 
         = "captifs_char_net_df$local_clustering < captifs$averageClustering")
show(captifs_char_net_df_eval_cc_av)
#####filter the selected character via logical vector to choose the schemer
captifs_char_net_df_eval_cc_av_schemer <-
  captifs_char_net_df_eval_cc_av %>%
  filter(row.names(captifs_char_net_df_eval_cc_av)
         %in% captifs_selected_characters)
show(captifs_char_net_df_eval_cc_av_schemer)

##[5.8.5.] create network graph

###[5.8.5.1.] layout network graph
###choose layout algorithm for graph
captifs_coocur_layout <- create_layout(captifs_coocur, layout = "stress")
###layout settings
captifs_coocur_layout <-
  ggraph(captifs_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(captifs_coocur_layout)
###export graph
ggsave(captifs_coocur_layout,
       file = "captifs_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

#[6.] analyse the early modern Italian comedies (ItaDraCor)

#[6.1.] Ariosto, La Cassaria

##[6.1.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
cassaria <- get_play_metadata(play = "ariosto-cassaria", 
                              corpus = "ita",
                              full_metadata = TRUE)
show(cassaria)
####create matrix with general values for the dramatic network
cassaria_gen_net_struc <- matrix(c(cassaria$size, cassaria$density,
                                   cassaria$diameter, cassaria$averageClustering,
                                   cassaria$averagePathLength,
                                   cassaria$averageDegree),
                                 ncol = 1, byrow = FALSE)
###convert matrix to data frame
cassaria_gen_net_struc_df <- as.data.frame(cassaria_gen_net_struc)
###specify columns and rows for the data frame
colnames(cassaria_gen_net_struc_df) <- c("value")
rownames(cassaria_gen_net_struc_df) <- c("size", "density", "diameter",
                                         "average clustering",
                                         "average path length", "average degree")
show(cassaria_gen_net_struc_df)
###create table with gt
gt_cassaria_gen_net_struc <- gt::gt(cassaria_gen_net_struc_df,
                                    rownames_to_stub = TRUE)
show(gt_cassaria_gen_net_struc)
####amend table
gt_cassaria_gen_net_struc <-
  gt_cassaria_gen_net_struc %>%
  #####add header
  tab_header(
    title = "La Cassaria",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_cassaria_gen_net_struc)
####export table
gtsave(gt_cassaria_gen_net_struc, "cassaria_gen_net_struc.png", zoom = 10)

##[6.1.2.] extract, calculate, and add character specific values
cassaria_coocur <- get_net_cooccur_igraph(play =
                                            "ariosto-cassaria",
                                          corpus = "ita")
class(cassaria_coocur)
###calculate local clustering
cassaria_local_clustering <- transitivity(
  cassaria_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
cassaria_coocur <- set_vertex_attr(cassaria_coocur, "local_clustering",
                                   value = cassaria_local_clustering)
###calculate triangles
cassaria_triangles <- count_triangles(cassaria_coocur)
###add triangles
cassaria_coocur <- set_vertex_attr(cassaria_coocur, "triangles",
                                   value = cassaria_triangles)
###show data
show(cassaria_coocur)
class(cassaria_coocur)
###export as data frame
cassaria_char_spec_v_df <- as_data_frame(cassaria_coocur, what="vertices")
show(cassaria_char_spec_v_df)

##[6.1.3.] create two tables with character specific values

###[6.1.3.1.] create table with count-based measures for each character
####extract table with count-based measures for each character
cassaria_char_sp_df <- select(cassaria_char_spec_v_df,
                              c(numOfWords, numOfSpeechActs, numOfScenes))
show(cassaria_char_sp_df)
####arrange rows by number of scenes
cassaria_char_sp_df <- arrange(cassaria_char_sp_df,
                               desc(numOfWords),
                               desc(numOfSpeechActs))
show(cassaria_char_sp_df)
####create table with gt
gt_cassaria_char_sp <- gt::gt(cassaria_char_sp_df, rownames_to_stub = TRUE)
show(gt_cassaria_char_sp)
####layout table
gt_cassaria_char_sp <-
  gt_cassaria_char_sp %>%
  #####add header
  tab_header(
    title = "La Cassaria",
    subtitle = "count-based measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(numOfWords = "words",
             numOfSpeechActs = "speech acts",
             numOfScenes = "scenes") %>%
  #####bold row showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Volpino")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Volpino"))
#####colour the table: the higher the value, the darker the cell
gt_cassaria_char_sp <-
  gt_cassaria_char_sp %>%
  gt_color_rows(numOfWords:numOfScenes,
                palette = RColorBrewer::brewer.pal(5,"GnBu"))
#####show final layout
show(gt_cassaria_char_sp)
#####set margin to prevent cut off
gt_cassaria_char_sp <-
  gt_cassaria_char_sp %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_cassaria_char_sp)
####export table
gtsave(gt_cassaria_char_sp, "cassaria_char_sp.png", zoom = 10)

###[6.1.3.2.] create table with character specific network values
###extract table with character specific network values
cassaria_char_net_df <- select(cassaria_char_spec_v_df,
                               c(degree, weightedDegree,
                                 closeness, betweenness,
                                 local_clustering, triangles))
show(cassaria_char_net_df)
####arrange rows by degree
cassaria_char_net_df <- arrange(cassaria_char_net_df, 
                                desc(degree),
                                desc(weightedDegree))
show(cassaria_char_net_df)
####create table with gt
gt_cassaria_char_net <- gt::gt(cassaria_char_net_df, rownames_to_stub = TRUE)
show(gt_cassaria_char_net)
####layout table
gt_cassaria_char_net <-
  gt_cassaria_char_net %>%
  #####add header
  tab_header(
    title = "La Cassaria",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.")
#####colour the table
gt_cassaria_char_net <-
  gt_cassaria_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Volpino")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Volpino"))
####show final layout
show(gt_cassaria_char_net)
#####set margin to prevent cut off
gt_cassaria_char_net <-
  gt_cassaria_char_net %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_cassaria_char_net)
####export table
gtsave(gt_cassaria_char_net, "cassaria_char_net.png", zoom = 10)

##[6.1.4.] evaluate the data regarding the hypotheses of the paper

###[6.1.4.1.] create a vector with the scheming characters 
cassaria_selected_characters <- c("Volpino")
show(cassaria_selected_characters)

###[6.1.4.2.] evaluate the count-based data

####[6.1.4.2.1.] check if the number of words, the number of speech acts, and 
####the number of scenes of the schemer are among the three best ranked
####values respectively
cassaria_char_sp_df_eval_3 <-
  cassaria_char_sp_df %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(cassaria_char_sp_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
cassaria_char_sp_df_eval_3_schemer <- 
  cassaria_char_sp_df_eval_3 %>%
  filter(row.names(cassaria_char_sp_df_eval_3) 
         %in% cassaria_selected_characters)
show(cassaria_char_sp_df_eval_3_schemer)

###[6.1.4.3.] evaluate the network data

####[6.1.4.3.1.] check if the degree centrality, weighted degree centrality,
####closeness centrality, betweenness centrality, and the number of triangles
####of the schemer are among the three best ranked values respectively.
####check if the clustering coefficient of the schemer
####is among the three lowest values.
cassaria_char_net_df_eval_3 <-
  cassaria_char_net_df %>%
  #####reverse values for local clustering
  mutate(local_clustering = -local_clustering) %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(cassaria_char_net_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
cassaria_char_net_df_eval_3_schemer <- 
  cassaria_char_net_df_eval_3 %>%
  filter(row.names(cassaria_char_net_df_eval_3) 
         %in% cassaria_selected_characters)
show(cassaria_char_net_df_eval_3_schemer)

####[6.1.4.3.2.] check if the degree centrality of the schemer is above
####average
cassaria_char_net_df_eval_dc_av <-
  cassaria_char_net_df %>%
  #####check if degree centrality is above average and extract column
  transmute(cassaria_char_net_df$degree > cassaria$averageDegree)
show(cassaria_char_net_df_eval_dc_av)
#####rename column
cassaria_char_net_df_eval_dc_av <-
  cassaria_char_net_df_eval_dc_av %>%
  rename("d.c. > av. d.c." = 
           "cassaria_char_net_df$degree > cassaria$averageDegree")
show(cassaria_char_net_df_eval_dc_av)
#####filter the selected character via logical vector to choose the schemer
cassaria_char_net_df_eval_dc_av_schemer <-
  cassaria_char_net_df_eval_dc_av %>%
  filter(row.names(cassaria_char_net_df_eval_dc_av) %in% cassaria_selected_characters)
show(cassaria_char_net_df_eval_dc_av_schemer)

####[6.1.4.3.3.] check if the clustering coefficient of the schemer is below
####average
cassaria_char_net_df_eval_cc_av <-
  cassaria_char_net_df %>%
  #####check if clustering coefficient is below average and extract column
  transmute(cassaria_char_net_df$local_clustering < cassaria$averageClustering)
show(cassaria_char_net_df_eval_cc_av)
#####rename column
cassaria_char_net_df_eval_cc_av <-
  cassaria_char_net_df_eval_cc_av %>%
  rename("c.c. < av. c.c." 
         = "cassaria_char_net_df$local_clustering < cassaria$averageClustering")
show(cassaria_char_net_df_eval_cc_av)
#####filter the selected character via logical vector to choose the schemer
cassaria_char_net_df_eval_cc_av_schemer <-
  cassaria_char_net_df_eval_cc_av %>%
  filter(row.names(cassaria_char_net_df_eval_cc_av) %in% cassaria_selected_characters)
show(cassaria_char_net_df_eval_cc_av_schemer)

##[6.1.5.] create network graph

###[6.1.5.1.] layout network graph
###choose layout algorithm for graph
cassaria_coocur_layout <- create_layout(cassaria_coocur, layout = "stress")
###layout settings
cassaria_coocur_layout <-
  ggraph(cassaria_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(cassaria_coocur_layout)
###export graph
ggsave(cassaria_coocur_layout,
       file = "cassaria_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

#[6.2.] Ariosto,	I Suppositi

##[6.2.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
suppositi <- get_play_metadata(play = "ariosto-i-suppositi", 
                               corpus = "ita",
                               full_metadata = TRUE)
show(suppositi)
####create matrix with general values for the dramatic network
suppositi_gen_net_struc <- matrix(c(suppositi$size, suppositi$density,
                                    suppositi$diameter, suppositi$averageClustering,
                                    suppositi$averagePathLength,
                                    suppositi$averageDegree),
                                  ncol = 1, byrow = FALSE)
###convert matrix to data frame
suppositi_gen_net_struc_df <- as.data.frame(suppositi_gen_net_struc)
###specify columns and rows for the data frame
colnames(suppositi_gen_net_struc_df) <- c("value")
rownames(suppositi_gen_net_struc_df) <- c("size", "density", "diameter",
                                          "average clustering",
                                          "average path length", "average degree")
show(suppositi_gen_net_struc_df)
###create table with gt
gt_suppositi_gen_net_struc <- gt::gt(suppositi_gen_net_struc_df,
                                     rownames_to_stub = TRUE)
show(gt_suppositi_gen_net_struc)
####amend table
gt_suppositi_gen_net_struc <-
  gt_suppositi_gen_net_struc %>%
  #####add header
  tab_header(
    title = "I Suppositi",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_suppositi_gen_net_struc)
####export table
gtsave(gt_suppositi_gen_net_struc, "suppositi_gen_net_struc.png", zoom = 10)

##[6.2.2.] extract, calculate, and add character specific values
suppositi_coocur <- get_net_cooccur_igraph(play =
                                             "ariosto-i-suppositi",
                                           corpus = "ita")
class(suppositi_coocur)
###calculate local clustering
suppositi_local_clustering <- transitivity(
  suppositi_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
suppositi_coocur <- set_vertex_attr(suppositi_coocur, "local_clustering",
                                    value = suppositi_local_clustering)
###calculate triangles
suppositi_triangles <- count_triangles(suppositi_coocur)
###add triangles
suppositi_coocur <- set_vertex_attr(suppositi_coocur, "triangles",
                                    value = suppositi_triangles)
###show data
show(suppositi_coocur)
class(suppositi_coocur)
###export as data frame
suppositi_char_spec_v_df <- as_data_frame(suppositi_coocur, what="vertices")
show(suppositi_char_spec_v_df)

##[6.2.3.] create two tables with character specific values

###[6.2.3.1.] create table with count-based measures for each character
####extract table with count-based measures for each character
suppositi_char_sp_df <- select(suppositi_char_spec_v_df,
                               c(numOfWords, numOfSpeechActs, numOfScenes))
show(suppositi_char_sp_df)
####arrange rows by number of scenes
suppositi_char_sp_df <- arrange(suppositi_char_sp_df,
                                desc(numOfWords),
                                desc(numOfSpeechActs))
show(suppositi_char_sp_df)
####create table with gt
gt_suppositi_char_sp <- gt::gt(suppositi_char_sp_df, rownames_to_stub = TRUE)
show(gt_suppositi_char_sp)
####layout table
gt_suppositi_char_sp <-
  gt_suppositi_char_sp %>%
  #####add header
  tab_header(
    title = "I Suppositi",
    subtitle = "count-based measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(numOfWords = "words",
             numOfSpeechActs = "speech acts",
             numOfScenes = "scenes") %>%
  #####bold row showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Dulippo")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Dulippo"))
#####colour the table: the higher the value, the darker the cell
gt_suppositi_char_sp <-
  gt_suppositi_char_sp %>%
  gt_color_rows(numOfWords:numOfScenes,
                palette = RColorBrewer::brewer.pal(5,"GnBu"))
#####show final layout
show(gt_suppositi_char_sp)
#####set margin to prevent cut off
gt_suppositi_char_sp <-
  gt_suppositi_char_sp %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_suppositi_char_sp)
####export table
gtsave(gt_suppositi_char_sp, "suppositi_char_sp.png", zoom = 10)

###[6.2.3.2.] create table with character specific network values
###extract table with character specific network values
suppositi_char_net_df <- select(suppositi_char_spec_v_df,
                                c(degree, weightedDegree,
                                  closeness, betweenness,
                                  local_clustering, triangles))
show(suppositi_char_net_df)
####arrange rows by degree
suppositi_char_net_df <- arrange(suppositi_char_net_df, 
                                 desc(degree),
                                 desc(weightedDegree))
show(suppositi_char_net_df)
####create table with gt
gt_suppositi_char_net <- gt::gt(suppositi_char_net_df, rownames_to_stub = TRUE)
show(gt_suppositi_char_net)
####layout table
gt_suppositi_char_net <-
  gt_suppositi_char_net %>%
  #####add header
  tab_header(
    title = "I Suppositi",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.")
#####colour the table
gt_suppositi_char_net <-
  gt_suppositi_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Dulippo")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Dulippo"))
####show final layout
show(gt_suppositi_char_net)
#####set margin to prevent cut off
gt_suppositi_char_net <-
  gt_suppositi_char_net %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_suppositi_char_net)
####export table
gtsave(gt_suppositi_char_net, "suppositi_char_net.png", zoom = 10)

##[6.2.4.] evaluate the data regarding the hypotheses of the paper

###[6.2.4.1.] create a vector with the scheming characters 
suppositi_selected_characters <- c("Dulippo")
show(suppositi_selected_characters)

###[6.2.4.2.] evaluate the count-based data

####[6.2.4.2.1.] check if the number of words, the number of speech acts, and 
####the number of scenes of the schemer are among the three best ranked
####values respectively
suppositi_char_sp_df_eval_3 <-
  suppositi_char_sp_df %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(suppositi_char_sp_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
suppositi_char_sp_df_eval_3_schemer <- 
  suppositi_char_sp_df_eval_3 %>%
  filter(row.names(suppositi_char_sp_df_eval_3) 
         %in% suppositi_selected_characters)
show(suppositi_char_sp_df_eval_3_schemer)

###[6.2.4.3.] evaluate the network data

####[6.2.4.3.1.] check if the degree centrality, weighted degree centrality,
####closeness centrality, betweenness centrality, and the number of triangles
####of the schemer are among the three best ranked values respectively.
####check if the clustering coefficient of the schemer
####is among the three lowest values.
suppositi_char_net_df_eval_3 <-
  suppositi_char_net_df %>%
  #####reverse values for local clustering
  mutate(local_clustering = -local_clustering) %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(suppositi_char_net_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
suppositi_char_net_df_eval_3_schemer <- 
  suppositi_char_net_df_eval_3 %>%
  filter(row.names(suppositi_char_net_df_eval_3) 
         %in% suppositi_selected_characters)
show(suppositi_char_net_df_eval_3_schemer)

####[6.2.4.3.2.] check if the degree centrality of the schemer is above
####average
suppositi_char_net_df_eval_dc_av <-
  suppositi_char_net_df %>%
  #####check if degree centrality is above average and extract column
  transmute(suppositi_char_net_df$degree > suppositi$averageDegree)
show(suppositi_char_net_df_eval_dc_av)
#####rename column
suppositi_char_net_df_eval_dc_av <-
  suppositi_char_net_df_eval_dc_av %>%
  rename("d.c. > av. d.c." = 
           "suppositi_char_net_df$degree > suppositi$averageDegree")
show(suppositi_char_net_df_eval_dc_av)
#####filter the selected character via logical vector to choose the schemer
suppositi_char_net_df_eval_dc_av_schemer <-
  suppositi_char_net_df_eval_dc_av %>%
  filter(row.names(suppositi_char_net_df_eval_dc_av) %in% suppositi_selected_characters)
show(suppositi_char_net_df_eval_dc_av_schemer)

####[6.2.4.3.3.] check if the clustering coefficient of the schemer is below
####average
suppositi_char_net_df_eval_cc_av <-
  suppositi_char_net_df %>%
  #####check if clustering coefficient is below average and extract column
  transmute(suppositi_char_net_df$local_clustering < suppositi$averageClustering)
show(suppositi_char_net_df_eval_cc_av)
#####rename column
suppositi_char_net_df_eval_cc_av <-
  suppositi_char_net_df_eval_cc_av %>%
  rename("c.c. < av. c.c." 
         = "suppositi_char_net_df$local_clustering < suppositi$averageClustering")
show(suppositi_char_net_df_eval_cc_av)
#####filter the selected character via logical vector to choose the schemer
suppositi_char_net_df_eval_cc_av_schemer <-
  suppositi_char_net_df_eval_cc_av %>%
  filter(row.names(suppositi_char_net_df_eval_cc_av) %in% suppositi_selected_characters)
show(suppositi_char_net_df_eval_cc_av_schemer)

##[6.2.5.] create network graph

###[6.2.5.1.] layout network graph
###choose layout algorithm for graph
suppositi_coocur_layout <- create_layout(suppositi_coocur, layout = "stress")
###layout settings
suppositi_coocur_layout <-
  ggraph(suppositi_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(suppositi_coocur_layout)
###export graph
ggsave(suppositi_coocur_layout,
       file = "suppositi_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")


#[6.3.] Ariosto,	La Lena

##[6.3.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
lena <- get_play_metadata(play = "ariosto-la-lena", 
                          corpus = "ita",
                          full_metadata = TRUE)
show(lena)
####create matrix with general values for the dramatic network
lena_gen_net_struc <- matrix(c(lena$size, lena$density,
                               lena$diameter, lena$averageClustering,
                               lena$averagePathLength,
                               lena$averageDegree),
                             ncol = 1, byrow = FALSE)
###convert matrix to data frame
lena_gen_net_struc_df <- as.data.frame(lena_gen_net_struc)
###specify columns and rows for the data frame
colnames(lena_gen_net_struc_df) <- c("value")
rownames(lena_gen_net_struc_df) <- c("size", "density", "diameter",
                                     "average clustering",
                                     "average path length", "average degree")
show(lena_gen_net_struc_df)
###create table with gt
gt_lena_gen_net_struc <- gt::gt(lena_gen_net_struc_df,
                                rownames_to_stub = TRUE)
show(gt_lena_gen_net_struc)
####amend table
gt_lena_gen_net_struc <-
  gt_lena_gen_net_struc %>%
  #####add header
  tab_header(
    title = "La Lena",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_lena_gen_net_struc)
####export table
gtsave(gt_lena_gen_net_struc, "lena_gen_net_struc.png", zoom = 10)

##[6.3.2.] extract, calculate, and add character specific values
lena_coocur <- get_net_cooccur_igraph(play =
                                        "ariosto-la-lena",
                                      corpus = "ita")
class(lena_coocur)
###calculate local clustering
lena_local_clustering <- transitivity(
  lena_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
lena_coocur <- set_vertex_attr(lena_coocur, "local_clustering",
                               value = lena_local_clustering)
###calculate triangles
lena_triangles <- count_triangles(lena_coocur)
###add triangles
lena_coocur <- set_vertex_attr(lena_coocur, "triangles",
                               value = lena_triangles)
###show data
show(lena_coocur)
class(lena_coocur)
###export as data frame
lena_char_spec_v_df <- as_data_frame(lena_coocur, what="vertices")
show(lena_char_spec_v_df)

##[6.3.3.] create two tables with character specific values

###[6.3.3.1.] create table with count-based measures for each character
####extract table with count-based measures for each character
lena_char_sp_df <- select(lena_char_spec_v_df,
                          c(numOfWords, numOfSpeechActs, numOfScenes))
show(lena_char_sp_df)
####arrange rows by number of scenes
lena_char_sp_df <- arrange(lena_char_sp_df,
                           desc(numOfWords),
                           desc(numOfSpeechActs))
show(lena_char_sp_df)
####create table with gt
gt_lena_char_sp <- gt::gt(lena_char_sp_df, rownames_to_stub = TRUE)
show(gt_lena_char_sp)
####layout table
gt_lena_char_sp <-
  gt_lena_char_sp %>%
  #####add header
  tab_header(
    title = "La Lena",
    subtitle = "count-based measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(numOfWords = "words",
             numOfSpeechActs = "speech acts",
             numOfScenes = "scenes") %>%
  #####bold row showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Corbolo")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Corbolo"))
#####colour the table: the higher the value, the darker the cell
gt_lena_char_sp <-
  gt_lena_char_sp %>%
  gt_color_rows(numOfWords:numOfScenes,
                palette = RColorBrewer::brewer.pal(5,"GnBu"))
#####show final layout
show(gt_lena_char_sp)
#####set margin to prevent cut off
gt_lena_char_sp <-
  gt_lena_char_sp %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_lena_char_sp)
####export table
gtsave(gt_lena_char_sp, "lena_char_sp.png", zoom = 10)

###[6.3.3.2.] create table with character specific network values
###extract table with character specific network values
lena_char_net_df <- select(lena_char_spec_v_df,
                           c(degree, weightedDegree,
                             closeness, betweenness,
                             local_clustering, triangles))
show(lena_char_net_df)
####arrange rows by degree
lena_char_net_df <- arrange(lena_char_net_df, 
                            desc(degree),
                            desc(weightedDegree))
show(lena_char_net_df)
####create table with gt
gt_lena_char_net <- gt::gt(lena_char_net_df, rownames_to_stub = TRUE)
show(gt_lena_char_net)
####layout table
gt_lena_char_net <-
  gt_lena_char_net %>%
  #####add header
  tab_header(
    title = "La Lena",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.")
#####colour the table
gt_lena_char_net <-
  gt_lena_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Corbolo")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Corbolo"))
####show final layout
show(gt_lena_char_net)
#####set margin to prevent cut off
gt_lena_char_net <-
  gt_lena_char_net %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_lena_char_net)
####export table
gtsave(gt_lena_char_net, "lena_char_net.png", zoom = 10)

##[6.3.4.] evaluate the data regarding the hypotheses of the paper

###[6.3.4.1.] create a vector with the scheming characters 
lena_selected_characters <- c("Corbolo")
show(lena_selected_characters)

###[6.3.4.2.] evaluate the count-based data

####[6.3.4.2.1.] check if the number of words, the number of speech acts, and 
####the number of scenes of the schemer are among the three best ranked
####values respectively
lena_char_sp_df_eval_3 <-
  lena_char_sp_df %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(lena_char_sp_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
lena_char_sp_df_eval_3_schemer <- 
  lena_char_sp_df_eval_3 %>%
  filter(row.names(lena_char_sp_df_eval_3) 
         %in% lena_selected_characters)
show(lena_char_sp_df_eval_3_schemer)

###[6.3.4.3.] evaluate the network data

####[6.3.4.3.1.] check if the degree centrality, weighted degree centrality,
####closeness centrality, betweenness centrality, and the number of triangles
####of the schemer are among the three best ranked values respectively.
####check if the clustering coefficient of the schemer
####is among the three lowest values.
lena_char_net_df_eval_3 <-
  lena_char_net_df %>%
  #####reverse values for local clustering
  mutate(local_clustering = -local_clustering) %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(lena_char_net_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
lena_char_net_df_eval_3_schemer <- 
  lena_char_net_df_eval_3 %>%
  filter(row.names(lena_char_net_df_eval_3) 
         %in% lena_selected_characters)
show(lena_char_net_df_eval_3_schemer)

####[6.3.4.3.2.] check if the degree centrality of the schemer is above
####average
lena_char_net_df_eval_dc_av <-
  lena_char_net_df %>%
  #####check if degree centrality is above average and extract column
  transmute(lena_char_net_df$degree > lena$averageDegree)
show(lena_char_net_df_eval_dc_av)
#####rename column
lena_char_net_df_eval_dc_av <-
  lena_char_net_df_eval_dc_av %>%
  rename("d.c. > av. d.c." = 
           "lena_char_net_df$degree > lena$averageDegree")
show(lena_char_net_df_eval_dc_av)
#####filter the selected character via logical vector to choose the schemer
lena_char_net_df_eval_dc_av_schemer <-
  lena_char_net_df_eval_dc_av %>%
  filter(row.names(lena_char_net_df_eval_dc_av) %in% lena_selected_characters)
show(lena_char_net_df_eval_dc_av_schemer)

####[6.3.4.3.3.] check if the clustering coefficient of the schemer is below
####average
lena_char_net_df_eval_cc_av <-
  lena_char_net_df %>%
  #####check if clustering coefficient is below average and extract column
  transmute(lena_char_net_df$local_clustering < lena$averageClustering)
show(lena_char_net_df_eval_cc_av)
#####rename column
lena_char_net_df_eval_cc_av <-
  lena_char_net_df_eval_cc_av %>%
  rename("c.c. < av. c.c." 
         = "lena_char_net_df$local_clustering < lena$averageClustering")
show(lena_char_net_df_eval_cc_av)
#####filter the selected character via logical vector to choose the schemer
lena_char_net_df_eval_cc_av_schemer <-
  lena_char_net_df_eval_cc_av %>%
  filter(row.names(lena_char_net_df_eval_cc_av) %in% lena_selected_characters)
show(lena_char_net_df_eval_cc_av_schemer)

##[6.3.5.] create network graph

###[6.3.5.1.] layout network graph
###choose layout algorithm for graph
lena_coocur_layout <- create_layout(lena_coocur, layout = "stress")
###layout settings
lena_coocur_layout <-
  ggraph(lena_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(lena_coocur_layout)
###export graph
ggsave(lena_coocur_layout,
       file = "lena_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

#[6.4.] Barbieri,	L’inavvertito

##[6.4.1.] create table with general values for the dramatic network
###extract general values for the dramatic network from the metadata
####get all metadata for the play
inavvertito <- get_play_metadata(play = "barbieri-l-inavertito", 
                                 corpus = "ita",
                                 full_metadata = TRUE)
show(inavvertito)
####create matrix with general values for the dramatic network
inavvertito_gen_net_struc <- matrix(c(inavvertito$size, inavvertito$density,
                                      inavvertito$diameter, inavvertito$averageClustering,
                                      inavvertito$averagePathLength,
                                      inavvertito$averageDegree),
                                    ncol = 1, byrow = FALSE)
###convert matrix to data frame
inavvertito_gen_net_struc_df <- as.data.frame(inavvertito_gen_net_struc)
###specify columns and rows for the data frame
colnames(inavvertito_gen_net_struc_df) <- c("value")
rownames(inavvertito_gen_net_struc_df) <- c("size", "density", "diameter",
                                            "average clustering",
                                            "average path length", "average degree")
show(inavvertito_gen_net_struc_df)
###create table with gt
gt_inavvertito_gen_net_struc <- gt::gt(inavvertito_gen_net_struc_df,
                                       rownames_to_stub = TRUE)
show(gt_inavvertito_gen_net_struc)
####amend table
gt_inavvertito_gen_net_struc <-
  gt_inavvertito_gen_net_struc %>%
  #####add header
  tab_header(
    title = "L’inavvertito",
    subtitle = "general network structure") %>%
  #####hide column labels
  tab_options(
    column_labels.hidden = TRUE)
####show table
show(gt_inavvertito_gen_net_struc)
####export table
gtsave(gt_inavvertito_gen_net_struc, "inavvertito_gen_net_struc.png", zoom = 10)

##[6.4.2.] extract, calculate, and add character specific values
inavvertito_coocur <- get_net_cooccur_igraph(play =
                                               "barbieri-l-inavertito",
                                             corpus = "ita")
class(inavvertito_coocur)
###calculate local clustering
inavvertito_local_clustering <- transitivity(
  inavvertito_coocur,
  type = "local",
  isolates = c("NaN"))
###add local clustering
inavvertito_coocur <- set_vertex_attr(inavvertito_coocur, "local_clustering",
                                      value = inavvertito_local_clustering)
###calculate triangles
inavvertito_triangles <- count_triangles(inavvertito_coocur)
###add triangles
inavvertito_coocur <- set_vertex_attr(inavvertito_coocur, "triangles",
                                      value = inavvertito_triangles)
###show data
show(inavvertito_coocur)
class(inavvertito_coocur)
###export as data frame
inavvertito_char_spec_v_df <- as_data_frame(inavvertito_coocur, what="vertices")
show(inavvertito_char_spec_v_df)

##[6.4.3.] create two tables with character specific values

###[6.4.3.1.] create table with count-based measures for each character
####extract table with count-based measures for each character
inavvertito_char_sp_df <- select(inavvertito_char_spec_v_df,
                                 c(numOfWords, numOfSpeechActs, numOfScenes))
show(inavvertito_char_sp_df)
####arrange rows by number of scenes
inavvertito_char_sp_df <- arrange(inavvertito_char_sp_df,
                                  desc(numOfWords),
                                  desc(numOfSpeechActs))
show(inavvertito_char_sp_df)
####create table with gt
gt_inavvertito_char_sp <- gt::gt(inavvertito_char_sp_df, rownames_to_stub = TRUE)
show(gt_inavvertito_char_sp)
####layout table
gt_inavvertito_char_sp <-
  gt_inavvertito_char_sp %>%
  #####add header
  tab_header(
    title = "L’inavvertito",
    subtitle = "count-based measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(numOfWords = "words",
             numOfSpeechActs = "speech acts",
             numOfScenes = "scenes") %>%
  #####bold row showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Scappino")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Scappino"))
#####colour the table: the higher the value, the darker the cell
gt_inavvertito_char_sp <-
  gt_inavvertito_char_sp %>%
  gt_color_rows(numOfWords:numOfScenes,
                palette = RColorBrewer::brewer.pal(5,"GnBu"))
#####show final layout
show(gt_inavvertito_char_sp)
#####set margin to prevent cut off
gt_inavvertito_char_sp <-
  gt_inavvertito_char_sp %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_inavvertito_char_sp)
####export table
gtsave(gt_inavvertito_char_sp, "inavvertito_char_sp.png", zoom = 10)

###[6.4.3.2.] create table with character specific network values
###extract table with character specific network values
inavvertito_char_net_df <- select(inavvertito_char_spec_v_df,
                                  c(degree, weightedDegree,
                                    closeness, betweenness,
                                    local_clustering, triangles))
show(inavvertito_char_net_df)
####arrange rows by degree
inavvertito_char_net_df <- arrange(inavvertito_char_net_df, 
                                   desc(degree),
                                   desc(weightedDegree))
show(inavvertito_char_net_df)
####create table with gt
gt_inavvertito_char_net <- gt::gt(inavvertito_char_net_df, rownames_to_stub = TRUE)
show(gt_inavvertito_char_net)
####layout table
gt_inavvertito_char_net <-
  gt_inavvertito_char_net %>%
  #####add header
  tab_header(
    title = "L’inavvertito",
    subtitle = "network measures") %>%
  #####add stubhead
  tab_stubhead(label = "character") %>%
  #####relabel columns
  cols_label(degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.")
#####colour the table
gt_inavvertito_char_net <-
  gt_inavvertito_char_net %>%
  #####the higher the value, the darker the cell
  gt_color_rows(degree:betweenness,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  gt_color_rows(triangles,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####the lower the value, the darker the cell
  gt_color_rows(local_clustering,
                reverse=TRUE,
                palette = RColorBrewer::brewer.pal(5,"GnBu")) %>%
  #####bold row showing the schemer
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = everything(),
              rows = "Scappino")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_stub(
              rows = "Scappino"))
####show final layout
show(gt_inavvertito_char_net)
#####set margin to prevent cut off
gt_inavvertito_char_net <-
  gt_inavvertito_char_net %>%
  tab_options(table.margin.left = 0.5,
              table.margin.right = 0.5)
show(gt_inavvertito_char_net)
####export table
gtsave(gt_inavvertito_char_net, "inavvertito_char_net.png", zoom = 10)

##[6.4.4.] evaluate the data regarding the hypotheses of the paper

###[6.4.4.1.] create a vector with the scheming characters 
inavvertito_selected_characters <- c("Scappino")
show(inavvertito_selected_characters)

###[6.4.4.2.] evaluate the count-based data

####[6.4.4.2.1.] check if the number of words, the number of speech acts, and 
####the number of scenes of the schemer are among the three best ranked
####values respectively
inavvertito_char_sp_df_eval_3 <-
  inavvertito_char_sp_df %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(inavvertito_char_sp_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
inavvertito_char_sp_df_eval_3_schemer <- 
  inavvertito_char_sp_df_eval_3 %>%
  filter(row.names(inavvertito_char_sp_df_eval_3) 
         %in% inavvertito_selected_characters)
show(inavvertito_char_sp_df_eval_3_schemer)

###[6.4.4.3.] evaluate the network data

####[6.4.4.3.1.] check if the degree centrality, weighted degree centrality,
####closeness centrality, betweenness centrality, and the number of triangles
####of the schemer are among the three best ranked values respectively.
####check if the clustering coefficient of the schemer
####is among the three lowest values.
inavvertito_char_net_df_eval_3 <-
  inavvertito_char_net_df %>%
  #####reverse values for local clustering
  mutate(local_clustering = -local_clustering) %>%
  mutate(
    #####use all numeric data of the data frame
    across(where(is.numeric),
           #####rank the data of the data frame
           function(x) dense_rank(-x)
           #####check if values rank at least third
           <= 3))
show(inavvertito_char_net_df_eval_3)
#####filter the selected character via logical vector to choose the schemer
inavvertito_char_net_df_eval_3_schemer <- 
  inavvertito_char_net_df_eval_3 %>%
  filter(row.names(inavvertito_char_net_df_eval_3) 
         %in% inavvertito_selected_characters)
show(inavvertito_char_net_df_eval_3_schemer)

####[6.4.4.3.2.] check if the degree centrality of the schemer is above
####average
inavvertito_char_net_df_eval_dc_av <-
  inavvertito_char_net_df %>%
  #####check if degree centrality is above average and extract column
  transmute(inavvertito_char_net_df$degree > inavvertito$averageDegree)
show(inavvertito_char_net_df_eval_dc_av)
#####rename column
inavvertito_char_net_df_eval_dc_av <-
  inavvertito_char_net_df_eval_dc_av %>%
  rename("d.c. > av. d.c." = 
           "inavvertito_char_net_df$degree > inavvertito$averageDegree")
show(inavvertito_char_net_df_eval_dc_av)
#####filter the selected character via logical vector to choose the schemer
inavvertito_char_net_df_eval_dc_av_schemer <-
  inavvertito_char_net_df_eval_dc_av %>%
  filter(row.names(inavvertito_char_net_df_eval_dc_av) %in% inavvertito_selected_characters)
show(inavvertito_char_net_df_eval_dc_av_schemer)

####[6.4.4.3.3.] check if the clustering coefficient of the schemer is below
####average
inavvertito_char_net_df_eval_cc_av <-
  inavvertito_char_net_df %>%
  #####check if clustering coefficient is below average and extract column
  transmute(inavvertito_char_net_df$local_clustering < inavvertito$averageClustering)
show(inavvertito_char_net_df_eval_cc_av)
#####rename column
inavvertito_char_net_df_eval_cc_av <-
  inavvertito_char_net_df_eval_cc_av %>%
  rename("c.c. < av. c.c." 
         = "inavvertito_char_net_df$local_clustering < inavvertito$averageClustering")
show(inavvertito_char_net_df_eval_cc_av)
#####filter the selected character via logical vector to choose the schemer
inavvertito_char_net_df_eval_cc_av_schemer <-
  inavvertito_char_net_df_eval_cc_av %>%
  filter(row.names(inavvertito_char_net_df_eval_cc_av) %in% inavvertito_selected_characters)
show(inavvertito_char_net_df_eval_cc_av_schemer)

##[6.4.5.] create network graph

###[6.4.5.1.] layout network graph
###choose layout algorithm for graph
inavvertito_coocur_layout <- create_layout(inavvertito_coocur, layout = "stress")
###layout settings
inavvertito_coocur_layout <-
  ggraph(inavvertito_coocur_layout) +
  ###define edge layout: the darker and wider the edge, the higher its weight
  geom_edge_link0(aes(edge_color = factor(weight), width = weight)) +
  ####define colour palette
  scale_edge_color_brewer(palette = "GnBu", aesthetics = "edge_color") +
  ####scale edge width
  scale_edge_width(range = c(5, 20)) +
  ####put less weighted edges in the background visually
  scale_edge_alpha(range = c(0.1, 1)) +
  ###define node layout
  geom_node_point(shape = 21, stroke = 0.2,
                  ### set border colour
                  color = "white",
                  ###the darker the node, the higher its unweighted degree
                  aes(fill = factor(degree),
                      ###the bigger the node, the higher its weighted degree
                      size = weightedDegree)) +
  ####define colour palette for nodes
  scale_fill_brewer(palette = "GnBu") +
  ####scale node size
  scale_size(range = c(50, 200)) +
  ###define text layout
  geom_node_text(color = "grey25", fontface = "plain",
                 position = "jitter",
                 ###prevent overlap
                 repel = TRUE,
                 check_overlap = TRUE,
                 aes(label = name,
                     size = (weightedDegree/200))) +
  ###prevent nodes from being cut off
  scale_x_continuous(expand = expansion(c(.10, .10))) +
  scale_y_continuous(expand = expansion(c(.10, .10))) +
  ###erase axes etc.
  theme_graph() +
  ###set no legend
  theme(legend.position = "none")
###control
show(inavvertito_coocur_layout)
###export graph
ggsave(inavvertito_coocur_layout,
       file = "inavvertito_net_graph.png",
       path = NULL,
       scale = 1,
       width = 1024,
       height = 1024,
       units = c("mm"),
       dpi = 300,
       limitsize = FALSE,
       bg = "transparent")

#[7.] carry out overall evaluation

##[7.1.] create table with the evaluation of the count-based measures

###[7.1.1.] create table showing if the schemers have at least the third best
###value
all_char_sp_df_eval_3_schemer <-
  ####choose all rows with schemers from the plays
  bind_rows(
    ####All Fools
    "All Fools" = fools_char_sp_df_eval_3_schemer,
    ####The English Traveller
    "The English Traveller" = traveller_char_sp_df_eval_3_schemer,
    ####The Case is Altered
    "The Case is Altered" = case_char_sp_df_eval_3_schemer,
    ####Every Man in His Humour
    "Every Man in His Humour" = man_char_sp_df_eval_3_schemer,
    ####Volpone
    "Volpone" = volpone_char_sp_df_eval_3_schemer,
    ####The Alchemist
    "The Alchemist" = alchemist_char_sp_df_eval_3_schemer,
    ####Ralph Roister Doister
    "Ralph Roister Doister" = ralph_char_sp_df_eval_3_schemer,
    ####Le barbier de Séville
    "Le barbier de Séville" = barbier_char_sp_df_eval_3_schemer,
    ####Le pédant joué
    "Le pédant joué" = pedant_char_sp_df_eval_3_schemer,
    ####Le véritable Capitan Matamore
    "Le véritable Capitan Matamore" = capitan_char_sp_df_eval_3_schemer,
    ####L’étourdi
    "L’étourdi" = etourdi_char_sp_df_eval_3_schemer,
    ####Les fourberies de Scapin
    "Les fourberies de Scapin" = scapin_char_sp_df_eval_3_schemer,
    ####Le retour imprévu
    "Le retour imprévu" = retour_char_sp_df_eval_3_schemer,
    ####La Sérénade
    "La sérénade" = serenade_char_sp_df_eval_3_schemer,
    ####Les Captifs
    "Les captifs" = captifs_char_sp_df_eval_3_schemer,
    ####La Cassaria
    "La Cassaria" = cassaria_char_sp_df_eval_3_schemer,
    ####I Suppositi
    "I Suppositi" = suppositi_char_sp_df_eval_3_schemer,
    ####La Lena
    "La Lena" = lena_char_sp_df_eval_3_schemer,
    ####L’inavvertito
    "L’inavvertito" = inavvertito_char_sp_df_eval_3_schemer,
    .id = "play")
show(all_char_sp_df_eval_3_schemer)

####amend data frame for layout
all_char_sp_df_eval_3_schemer_layout <-
  all_char_sp_df_eval_3_schemer %>%
  #####convert row names to column
  setDT(keep.rownames = "id") %>%
  #####relocate second column
  relocate(play, .before = id) 
show(all_char_sp_df_eval_3_schemer_layout)

####create table with gt
gt_all_char_sp_eval_3_schemer<-
  gt::gt(all_char_sp_df_eval_3_schemer_layout,
         rownames_to_stub = FALSE)
show(gt_all_char_sp_eval_3_schemer)
####layout table
gt_all_char_sp_eval_3_schemer_layout <-
  gt_all_char_sp_eval_3_schemer %>%
  #####highlight "TRUE" green
  data_color(columns = c(numOfWords,
                         numOfSpeechActs,
                         numOfScenes),
             colors = scales::col_factor(
               palette = c("white", "#b0f2c2"),
               domain = c(FALSE, TRUE)
             ),
             apply_to = "fill",
             autocolor_text = FALSE) %>%
  #####add header
  tab_header(
    title = "Do the schemers in early modern comedies have at least the third best value?",
    subtitle = "count-based measures") %>%
  #####relabel columns
  cols_label(id = "character",
             numOfWords = "words",
             numOfSpeechActs = "speech acts",
             numOfScenes = "scenes")
####show table
show(gt_all_char_sp_eval_3_schemer_layout)
####export table
gtsave(gt_all_char_sp_eval_3_schemer_layout,
       "all_char_sp_eval_3_schemer.png", 
       vwidth = 1500,
       vheight = 2000,
       zoom = 10)

##[7.2.] create table with the evaluation of the network measures

###[7.2.1.] create table showing if the schemers have at least the third best
###value
all_char_net_df_eval_3_schemer <-
  ####choose all rows with schemers from the plays
  bind_rows(
    ####All Fools
    "All Fools" = fools_char_net_df_eval_3_schemer,
    ####The English Traveller
    "The English Traveller" = traveller_char_net_df_eval_3_schemer,
    ####La Lena
    "The Case is Altered" = case_char_net_df_eval_3_schemer,
    ####Every Man in His Humour
    "Every Man in His Humour" = man_char_net_df_eval_3_schemer,
    ####Volpone
    "Volpone" = volpone_char_net_df_eval_3_schemer,
    ####The Alchemist
    "The Alchemist" = alchemist_char_net_df_eval_3_schemer,
    ####Ralph Roister Doister
    "Ralph Roister Doister" = ralph_char_net_df_eval_3_schemer,
    ####Le barbier de Séville
    "Le barbier de Séville" = barbier_char_net_df_eval_3_schemer,
    ####Le pédant joué
    "Le pédant joué" = pedant_char_net_df_eval_3_schemer,
    ####Le véritable Capitan Matamore
    "Le véritable Capitan Matamore" = capitan_char_net_df_eval_3_schemer,
    ####L’étourdi
    "L’étourdi" = etourdi_char_net_df_eval_3_schemer,
    ####Les fourberies de Scapin
    "Les fourberies de Scapin" = scapin_char_net_df_eval_3_schemer,
    ####Le retour imprévu
    "Le retour imprévu" = retour_char_net_df_eval_3_schemer,
    ####La Sérénade
    "La sérénade" = serenade_char_net_df_eval_3_schemer,
    ####Les Captifs
    "Les captifs" = captifs_char_net_df_eval_3_schemer,
    ####La Cassaria
    "La Cassaria" = cassaria_char_net_df_eval_3_schemer,
    ####I Suppositi
    "I Suppositi" = suppositi_char_net_df_eval_3_schemer,
    ####La Lena
    "La Lena" = lena_char_net_df_eval_3_schemer,
    ####L’inavvertito
    "L’inavvertito" = inavvertito_char_net_df_eval_3_schemer,
    .id = "play")
show(all_char_net_df_eval_3_schemer)

####amend data frame for layout
all_char_net_df_eval_3_schemer_layout <-
  all_char_net_df_eval_3_schemer %>%
  #####convert row names to column
  setDT(keep.rownames = "id") %>%
  #####relocate second column
  relocate(play, .before = id) 
show(all_char_net_df_eval_3_schemer_layout)

####create table with gt
gt_all_char_net_eval_3_schemer <-
  gt::gt(all_char_net_df_eval_3_schemer_layout,
         rownames_to_stub = FALSE)
show(gt_all_char_net_eval_3_schemer)
####layout table
gt_all_char_net_eval_3_schemer_layout <-
  gt_all_char_net_eval_3_schemer %>%
  #####highlight "TRUE" green
  data_color(columns = c(degree,
                         weightedDegree,
                         closeness,
                         betweenness,
                         local_clustering,
                         triangles),
             colors = scales::col_factor(
               palette = c("white", "#b0f2c2"),
               domain = c(FALSE, TRUE)
             ),
             apply_to = "fill",
             autocolor_text = FALSE) %>%
  #####add header
  tab_header(
    title = "Do the schemers in early modern comedies have at least the third best value?",
    subtitle = "network measures") %>%
  #####relabel columns
  cols_label(id = "character",
             degree = "d.c.",
             weightedDegree = "w.d.c.",
             closeness = "clos.c.",
             betweenness = "b.c.",
             local_clustering ="clus.c.",
             triangles ="tr.")
####show table
show(gt_all_char_net_eval_3_schemer_layout)
####export table
gtsave(gt_all_char_net_eval_3_schemer_layout,
       "all_char_net_eval_3_schemer.png", 
       vwidth = 1500,
       vheight = 2000,
       zoom = 10)

###[7.2.3.] check if the degree centrality of the schemers is above average

all_char_net_df_eval_dc_av_schemer <-
  ####choose all rows with schemers from the plays
  bind_rows(
    ####All Fools
    "All Fools" = fools_char_net_df_eval_dc_av_schemer,
    ####The English Traveller
    "The English Traveller" = traveller_char_net_df_eval_dc_av_schemer,
    ####La Lena
    "The Case is Altered" = case_char_net_df_eval_dc_av_schemer,
    ####Every Man in His Humour
    "Every Man in His Humour" = man_char_net_df_eval_dc_av_schemer,
    ####Volpone
    "Volpone" = volpone_char_net_df_eval_dc_av_schemer,
    ####The Alchemist
    "The Alchemist" = alchemist_char_net_df_eval_dc_av_schemer,
    ####Ralph Roister Doister
    "Ralph Roister Doister" = ralph_char_net_df_eval_dc_av_schemer,
    ####Le barbier de Séville
    "Le barbier de Séville" = barbier_char_net_df_eval_dc_av_schemer,
    ####Le pédant joué
    "Le pédant joué" = pedant_char_net_df_eval_dc_av_schemer,
    ####Le véritable Capitan Matamore
    "Le véritable Capitan Matamore" = capitan_char_net_df_eval_dc_av_schemer,
    ####L’étourdi
    "L’étourdi" = etourdi_char_net_df_eval_dc_av_schemer,
    ####Les fourberies de Scapin
    "Les fourberies de Scapin" = scapin_char_net_df_eval_dc_av_schemer,
    ####Le retour imprévu
    "Le retour imprévu" = retour_char_net_df_eval_dc_av_schemer,
    ####La Sérénade
    "La sérénade" = serenade_char_net_df_eval_dc_av_schemer,
    ####Les Captifs
    "Les captifs" = captifs_char_net_df_eval_dc_av_schemer,
    ####La Cassaria
    "La Cassaria" = cassaria_char_net_df_eval_dc_av_schemer,
    ####I Suppositi
    "I Suppositi" = suppositi_char_net_df_eval_dc_av_schemer,
    ####La Lena
    "La Lena" = lena_char_net_df_eval_dc_av_schemer,
    ####L’inavvertito
    "L’inavvertito" = inavvertito_char_net_df_eval_dc_av_schemer,
    .id = "play")
show(all_char_net_df_eval_dc_av_schemer)

####amend data frame for layout
all_char_net_df_eval_dc_av_schemer_layout <-
  all_char_net_df_eval_dc_av_schemer %>%
  #####convert row names to column
  setDT(keep.rownames = "id") %>%
  #####relocate second column
  relocate(play, .before = id) 
show(all_char_net_df_eval_dc_av_schemer_layout)

####create table with gt
gt_all_char_net_eval_dc_av_schemer <-
  gt::gt(all_char_net_df_eval_dc_av_schemer_layout,
         rownames_to_stub = FALSE)
show(gt_all_char_net_eval_dc_av_schemer)
####layout table
gt_all_char_net_eval_dc_av_schemer_layout <-
  gt_all_char_net_eval_dc_av_schemer %>%
  #####highlight "TRUE" green
  data_color(columns = c("d.c. > av. d.c."),
             colors = scales::col_factor(
               palette = c("white", "#b0f2c2"),
               domain = c(FALSE, TRUE)
             ),
             apply_to = "fill",
             autocolor_text = FALSE) %>%
  #####add header
  tab_header(
    title = "Do the schemers in early modern comedies have a degree centrality above average?",
    subtitle = "network measures") %>%
  #####relabel columns
  cols_label(id = "character") %>%
  #####set vertical line
  gt_add_divider(columns = 2,
                 color = "lightgrey",
                 style = "solid")
####show table
show(gt_all_char_net_eval_dc_av_schemer_layout)
####export table
gtsave(gt_all_char_net_eval_dc_av_schemer_layout,
       "all_char_net_eval_dc_av_schemer.png", 
       vwidth = 1500,
       vheight = 2000,
       zoom = 10)

###[7.2.3.] check if the clustering coefficient of the schemers is below
###average
all_char_net_df_eval_cc_av_schemer <-
  ####choose all rows with schemers from the plays
  bind_rows(
    ####All Fools
    "All Fools" = fools_char_net_df_eval_cc_av_schemer,
    ####The English Traveller
    "The English Traveller" = traveller_char_net_df_eval_cc_av_schemer,
    ####La Lena
    "The Case is Altered" = case_char_net_df_eval_cc_av_schemer,
    ####Every Man in His Humour
    "Every Man in His Humour" = man_char_net_df_eval_cc_av_schemer,
    ####Volpone
    "Volpone" = volpone_char_net_df_eval_cc_av_schemer,
    ####The Alchemist
    "The Alchemist" = alchemist_char_net_df_eval_cc_av_schemer,
    ####Ralph Roister Doister
    "Ralph Roister Doister" = ralph_char_net_df_eval_cc_av_schemer,
    ####Le barbier de Séville
    "Le barbier de Séville" = barbier_char_net_df_eval_cc_av_schemer,
    ####Le pédant joué
    "Le pédant joué" = pedant_char_net_df_eval_cc_av_schemer,
    ####Le véritable Capitan Matamore
    "Le véritable Capitan Matamore" = capitan_char_net_df_eval_cc_av_schemer,
    ####L’étourdi
    "L’étourdi" = etourdi_char_net_df_eval_cc_av_schemer,
    ####Les fourberies de Scapin
    "Les fourberies de Scapin" = scapin_char_net_df_eval_cc_av_schemer,
    ####Le retour imprévu
    "Le retour imprévu" = retour_char_net_df_eval_cc_av_schemer,
    ####La Sérénade
    "La sérénade" = serenade_char_net_df_eval_cc_av_schemer,
    ####Les Captifs
    "Les captifs" = captifs_char_net_df_eval_cc_av_schemer,
    ####La Cassaria
    "La Cassaria" = cassaria_char_net_df_eval_cc_av_schemer,
    ####I Suppositi
    "I Suppositi" = suppositi_char_net_df_eval_cc_av_schemer,
    ####La Lena
    "La Lena" = lena_char_net_df_eval_cc_av_schemer,
    ####L’inavvertito
    "L’inavvertito" = inavvertito_char_net_df_eval_cc_av_schemer,
    .id = "play")
show(all_char_net_df_eval_cc_av_schemer)

####amend data frame for layout
all_char_net_df_eval_cc_av_schemer_layout <-
  all_char_net_df_eval_cc_av_schemer %>%
  #####convert row names to column
  setDT(keep.rownames = "id") %>%
  #####relocate second column
  relocate(play, .before = id) 
show(all_char_net_df_eval_cc_av_schemer_layout)

####create table with gt
gt_all_char_net_eval_cc_av_schemer <-
  gt::gt(all_char_net_df_eval_cc_av_schemer_layout,
         rownames_to_stub = FALSE)
show(gt_all_char_net_eval_cc_av_schemer)
####layout table
gt_all_char_net_eval_cc_av_schemer_layout <-
  gt_all_char_net_eval_cc_av_schemer %>%
  #####highlight "TRUE" green
  data_color(columns = c("c.c. < av. c.c."),
             colors = scales::col_factor(
               palette = c("white", "#b0f2c2"),
               domain = c(FALSE, TRUE)
             ),
             apply_to = "fill",
             autocolor_text = FALSE) %>%
  #####add header
  tab_header(
    title = "Do the schemers in early modern comedies have a clustering coefficient below average?",
    subtitle = "network measures") %>%
  #####relabel columns
  cols_label(id = "character") %>%
  #####set vertical line
  gt_add_divider(columns = 2,
                 color = "lightgrey",
                 style = "solid")
####show table
show(gt_all_char_net_eval_cc_av_schemer_layout)
####export table
gtsave(gt_all_char_net_eval_cc_av_schemer_layout,
       "all_char_net_eval_cc_av_schemer.png", 
       vwidth = 1500,
       vheight = 2000,
       zoom = 10)