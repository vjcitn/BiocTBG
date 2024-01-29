#' produce EntitySchema
#' @param num_partitions integer(1)
#' @param featurized logical(1)
#' @param dimension NULL Or integer(1)
#' @param pbgref instance of torchbiggraph module
#' @examples
#' pbg = reticulate::import("torchbiggraph")
#' EntitySchema(pbgref = pbg)
#' @export
make_entity_schema = function(num_partitions = 1L,
  featurized=FALSE, dimension=NULL, pbgref)
 pbgref$config$EntitySchema(num_partitions = num_partitions,
   featurized=featurized, dimension=dimension)

#' make ConfigSchema
#' @param pbgref instance of torchbiggraph module
#' @param entities EntitySchema instance
#' @param relations list of RelationSchema instances
#' @examples
#' pbgref = reticulate::import("torchbiggraph")
#' ent = make_entity_schema(pbgref=pbgref)
#' entities=reticulate::dict(all=ent)
#' rs = BiocPBG::make_rel_schema(pbgref=pbgref)
#' cs = setup_config_schema( pbgref = pbgref, entities = entities,
#'    relations = list(rs), entity_path = tempdir(), edge_paths = c("tr", "va", "te"),
#'    checkpoint_path = "cp" )
#' cs
#' @export
setup_config_schema = function( pbgref, entities, relations, entity_path,
 edge_paths, checkpoint_path,
 dimension=400L, init_scale=0.001, max_norm=NULL, global_emb=FALSE, comparator='dot',
 bias=FALSE, loss_fn='softmax', margin=0.1, regularization_coef=0.001, regularizer='N3',
 init_path=NULL, checkpoint_preservation_interval=NULL, num_epochs=50L,
 num_edge_chunks=NULL, max_edges_per_chunk=1000000000L, 
 bucket_order=pbgref$config$BucketOrder$INSIDE_OUT,
 workers=NULL, batch_size=1000L, num_batch_negs=50L, num_uniform_negs=1000L,
 disable_lhs_negs=FALSE, disable_rhs_negs=FALSE, lr=0.1, relation_lr=NULL,
 eval_fraction=0.0, eval_num_batch_negs=1000L, eval_num_uniform_negs=1000L, background_io=FALSE,
 verbose=0L, hogwild_delay=2.0, dynamic_relations=TRUE, num_machines=1L,
 num_partition_servers=-1L, distributed_init_method=NULL, distributed_tree_init_order=TRUE, num_gpus=0L,
 num_groups_for_partition_server=16L, num_groups_per_sharded_partition_server=1L, 
 partition_shard_size=250L, half_precision=FALSE) {
 pbgref$config$ConfigSchema(
  entities = entities, relations=relations,
  entity_path = entity_path, edge_paths = edge_paths, checkpoint_path = checkpoint_path,
  dimension=dimension,
  init_scale=init_scale,
  global_emb=global_emb,
  comparator=comparator,
  bias=bias,
  loss_fn=loss_fn,
  margin=margin,
  regularization_coef=regularization_coef,
  regularizer=regularizer,
  num_epochs=num_epochs,
  max_edges_per_chunk=max_edges_per_chunk,
  bucket_order = bucket_order,
  batch_size=batch_size,
  num_batch_negs=num_batch_negs,
  num_uniform_negs=num_uniform_negs,
  disable_lhs_negs=disable_lhs_negs,
  disable_rhs_negs=disable_rhs_negs,
  lr=lr,
  eval_fraction=eval_fraction,
  eval_num_batch_negs=eval_num_batch_negs,
  eval_num_uniform_negs=eval_num_uniform_negs,
  background_io=background_io,
  verbose=verbose,
  hogwild_delay=hogwild_delay,
  dynamic_relations=dynamic_relations,
  num_machines=num_machines,
  num_partition_servers=num_partition_servers,
  distributed_tree_init_order=distributed_tree_init_order,
  num_gpus=num_gpus,
  num_groups_for_partition_server=num_groups_for_partition_server,
  num_groups_per_sharded_partition_server=num_groups_per_sharded_partition_server
)
}
#
#class ConfigSchema(torchbiggraph.schema.Schema)
# |  ConfigSchema(*,
# entities: Dict[str,
# torchbiggraph.config.EntitySchema],
# relations: List[torchbiggraph.config.RelationSchema],
# dimension: int,
# init_scale: float = 0.001,
# max_norm: Optional[float] = None,
# global_emb: bool = True,
# comparator: str = 'cos',
# bias: bool = False,
# loss_fn: str = 'ranking',
# margin: float = 0.1,
# regularization_coef: float = 0,
# regularizer: str = 'N3',
# entity_path: str,
# edge_paths: List[str],
# checkpoint_path: str,
# init_path: Optional[str] = None,
# checkpoint_preservation_interval: Optional[int] = None,
# num_epochs: int = 1,
# num_edge_chunks: Optional[int] = None,
# max_edges_per_chunk: int = 1000000000,
# bucket_order: torchbiggraph.config.BucketOrder = <BucketOrder.INSIDE_OUT: 'inside_out'>,
# workers: Optional[int] = None,
# batch_size: int = 1000,
# num_batch_negs: int = 50,
# num_uniform_negs: int = 50,
# disable_lhs_negs: bool = False,
# disable_rhs_negs: bool = False,
# lr: float = 0.01,
# relation_lr: Optional[float] = None,
# eval_fraction: float = 0.05,
# eval_num_batch_negs: Optional[int] = 1000,
# eval_num_uniform_negs: Optional[int] = 1000,
# background_io: bool = False,
# verbose: int = 0,
# hogwild_delay: float = 2,
# dynamic_relations: bool = False,
# num_machines: int = 1,
# num_partition_servers: int = -1,
# distributed_init_method: Optional[str] = None,
# distributed_tree_init_order: bool = True,
# num_gpus: int = 0,
# num_groups_for_partition_server: int = 16,
# num_groups_per_sharded_partition_server: int = 1,
# partition_shard_size: int = 250,
# half_precision: bool = False) -> None
#
