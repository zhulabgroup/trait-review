import pandas as pd
import textnets as tn

## pip install spacy
## python -m spacy download en_core_web_sm
import spacy
# Load the language model
nlp = spacy.load('en_core_web_sm')
# Increase the max_length limit
nlp.max_length = 2000000

tn.params["seed"] = 42


dat_bib = pd.read_csv('GitHub/traitlitreview/data/df_phrase_para.csv', index_col= "id") 
dat_bib


corpus = tn.Corpus(dat_bib["text"])
corpus


remove_words = ["rights__reserved", "results__showed","springer__nature","present__study",
                "significant__differences", "significant__effect","rights__reserve","result__shows",
               "climate__change"]
corpus_token = corpus.tokenized(remove=remove_words,
                                remove_punctuation=True, 
                                stem=True,
                                remove_stop_words=True)



t = tn.Textnet(corpus_token,#doc_attrs=dat_bib[["code"]].to_dict(),
               min_docs=100,remove_weak_edges=True)
t



category_colors = {
    "1100" :"red",
    "1102" :"purple",
    "1103" :"orange",
    "1104": "pink",
    "1105":"yellow",
    "1106":"black",
    "1107":"blue",
    "1108":"green",
    "1109":"coral",
    "1110":"brown",
    "1111":"white",
    "1300":"cyan",
    "2400": "magenta",
    "2401": "olive",
    "2800":"gold",
    "3000":"azure",
    "EARTH":"navy",
    "ENVI" : "indigo",
    "HEALTH":"aquamarine",
    "SOCI":"silver"
}
dat_bib["color"] = [category_colors[category] for category in dat_bib["code"]]




def map_list_with_default(input_list, mapping_dict, default_value):
    new_list = [mapping_dict[item] if item in mapping_dict else default_value for item in input_list]
    return new_list



color_list = map_list_with_default(input_list= t.nodes().get_attribute_values("id"),
                                   mapping_dict = id_colors,
                                   default_value = "gray")

t.plot(show_clusters=False,
       label_term_nodes = True,
       label_doc_nodes = False,
      scale_nodes_by="birank",
      scale_edges_by="weight",
       node_opacity = 0.2,
      edge_opacity=0.1,
       vertex_color=color_list
)




code = t.project(node_type=tn.DOC)
color_list = map_list_with_default(input_list= code.nodes().get_attribute_values("id"),
                                   mapping_dict = id_colors,
                                   default_value = "gray")
code.plot(label_nodes=False,
          node_opacity = 0.2,
      edge_opacity=0.1,
         vertex_color=color_list)





phrase = t.project(node_type=tn.TERM)
color_list = map_list_with_default(input_list= phrase.nodes().get_attribute_values("id"),
                                   mapping_dict = id_colors,
                                   default_value = "gray")
phrase.plot(label_nodes=True,
           show_clusters=True,
            node_opacity = 0.2,
      edge_opacity=0.1,
         vertex_color=color_list)