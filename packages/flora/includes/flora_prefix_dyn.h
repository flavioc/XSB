
#define FLORA_ISA(X,Y)       	    '_$_$_flora_isa_dyn'(X,Y)
#define FLORA_ISA_RHS(X,Y)   	    '_$_$_flora_isa_rhs_dyn'(X,Y)
#define FLORA_SUBCLASS(X,Y)  	    '_$_$_flora_subclass_dyn'(X,Y)
#define FLORA_SUB(X,Y)       	    '_$_$_flora_sub_dyn'(X,Y)
#define FLORA_SUB_RHS(X,Y)   	    '_$_$_flora_sub_rhs_dyn'(X,Y)
#define FLORA_STRICT_SUBCLASS(X,Y)  '_$_$_flora_strict_subclass_dyn'(X,Y)


#define FLORA_FD(O,M,V)     '_$_$_flora_fd_dyn'(O,M,V)
#define FLORA_MVD(O,M,V)    '_$_$_flora_mvd_dyn'(O,M,V)
#define FLORA_IFD(O,M,V)    '_$_$_flora_ifd_dyn'(O,M,V)
#define FLORA_IMVD(O,M,V)   '_$_$_flora_imvd_dyn'(O,M,V)

#define FLORA_FS(O,M,V)     '_$_$_flora_fs_dyn'(O,M,V)
#define FLORA_MVS(O,M,V)    '_$_$_flora_mvs_dyn'(O,M,V)
#define FLORA_IFS(O,M,V)    '_$_$_flora_ifs_dyn'(O,M,V)
#define FLORA_IMVS(O,M,V)   '_$_$_flora_imvs_dyn'(O,M,V)

#define FLORA_FD_RHS(O,M,V)      '_$_$_flora_fd_rhs_dyn'(O,M,V)
#define FLORA_MVD_RHS(O,M,V)     '_$_$_flora_mvd_rhs_dyn'(O,M,V)
#define FLORA_IFD_RHS(O,M,V)     '_$_$_flora_ifd_rhs_dyn'(O,M,V)
#define FLORA_IMVD_RHS(O,M,V)    '_$_$_flora_imvd_rhs_dyn'(O,M,V)

#define FLORA_FS_RHS(O,M,V)      '_$_$_flora_fs_rhs_dyn'(O,M,V)
#define FLORA_MVS_RHS(O,M,V)     '_$_$_flora_mvs_rhs_dyn'(O,M,V)
#define FLORA_IFS_RHS(O,M,V)     '_$_$_flora_ifs_rhs_dyn'(O,M,V)
#define FLORA_IMVS_RHS(O,M,V)    '_$_$_flora_imvs_rhs_dyn'(O,M,V)


#define FLORA_DEFINED_FD(O,M)      '_$_$_flora_defined_fd_dyn'(O,M)
#define FLORA_DEFINED_MVD(O,M)     '_$_$_flora_defined_mvd_dyn'(O,M)
#define FLORA_DEFINED_IFD(O,M)     '_$_$_flora_defined_ifd_dyn'(O,M)
#define FLORA_DEFINED_IMVD(O,M)    '_$_$_flora_defined_imvd_dyn'(O,M)


#define FLORA_OVERWRITTEN_FD(O,C,M)    '_$_$_flora_overwritten_fd_dyn'(O,C,M)
#define FLORA_OVERWRITTEN_MVD(O,C,M)   '_$_$_flora_overwritten_mvd_dyn'(O,C,M)
#define FLORA_OVERWRITTEN_IFD(O,C,M)   '_$_$_flora_overwritten_ifd_dyn'(O,C,M)
#define FLORA_OVERWRITTEN_IMVD(O,C,M)  '_$_$_flora_overwritten_imvd_dyn'(O,C,M)


#define FLORA_CONFLICT_FD(C,S,M)    '_$_$_flora_conflict_fd_dyn'(C,S,M)
#define FLORA_CONFLICT_MVD(C,S,M)   '_$_$_flora_conflict_mvd_dyn'(C,S,M)
#define FLORA_CONFLICT_IFD(C,S,M)   '_$_$_flora_conflict_ifd_dyn'(C,S,M)
#define FLORA_CONFLICT_IMVD(C,S,M)  '_$_$_flora_conflict_imvd_dyn'(C,S,M)


#define FLORA_EXISTS(O)	        '_$_$_flora_exists_dyn'(O)
#define FLORA_EQL(O1,O2)        '_$_$_flora_eql_dyn'(O1,O2)
#define FLORA_METHEQL(M1,M2)    '_$_$_flora_metheql_dyn'(M1,M2)
#define FLORA_ARGEQL(A1,A2,V)   '_$_$_flora_argeql_dyn'(A1,A2,V)
