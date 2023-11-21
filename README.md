
# EQUI-Manager

In the left-side container, a tree list of functional locations is displayed. When a node in the tree is double-clicked, the right side shows all equipment associated with that functional location. Clicking on an equipment number triggers a call to transaction IE03, displaying detailed information about the equipment



## Design pattern
+ Singleton

## GUI Component

+ cl_gui_container
+ cl_gui_splitter_container
+ cl_salv_table
+ cl_salv_events_table
+ cl_salv_tree
+ cl_salv_events_tree
+ salv_de_tree_image


## PM Tables


| Table | Description                |
| :--------  | :------------------------- |
| `EQUI` | Equipment master data |
| `EQKT` | Equipment Short Texts |
| `EQUZ` | Equipment time segment |
| `ILOA` | PM Object Location |
| `IFLOT` | Functional Location / Short Texts |

