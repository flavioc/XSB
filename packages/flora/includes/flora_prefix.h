

#define FLORA_ISA(X,Y)              '_$_$_flora_isa'(X,Y)
#define FLORA_ISA_RHS(X,Y)          '_$_$_flora_isa_rhs'(X,Y)
#define FLORA_SUBCLASS(X,Y)         '_$_$_flora_subclass'(X,Y)
#define FLORA_SUB(X,Y)              '_$_$_flora_sub'(X,Y)
#define FLORA_SUB_RHS(X,Y)          '_$_$_flora_sub_rhs'(X,Y)
#define FLORA_STRICT_SUBCLASS(X,Y)  '_$_$_flora_strict_subclass'(X,Y)


#define FLORA_FD(O,M,V)    '_$_$_flora_fd'(O,M,V)
#define FLORA_MVD(O,M,V)   '_$_$_flora_mvd'(O,M,V)
#define FLORA_IFD(O,M,V)   '_$_$_flora_ifd'(O,M,V)
#define FLORA_IMVD(O,M,V)  '_$_$_flora_imvd'(O,M,V)

#define FLORA_FS(O,M,V)    '_$_$_flora_fs'(O,M,V)
#define FLORA_MVS(O,M,V)   '_$_$_flora_mvs'(O,M,V)
#define FLORA_IFS(O,M,V)   '_$_$_flora_ifs'(O,M,V)
#define FLORA_IMVS(O,M,V)  '_$_$_flora_imvs'(O,M,V)

#define FLORA_FD_RHS(O,M,V)     '_$_$_flora_fd_rhs'(O,M,V)
#define FLORA_MVD_RHS(O,M,V)    '_$_$_flora_mvd_rhs'(O,M,V)
#define FLORA_IFD_RHS(O,M,V)    '_$_$_flora_ifd_rhs'(O,M,V)
#define FLORA_IMVD_RHS(O,M,V)   '_$_$_flora_imvd_rhs'(O,M,V)

#define FLORA_FS_RHS(O,M,V)     '_$_$_flora_fs_rhs'(O,M,V)
#define FLORA_MVS_RHS(O,M,V)    '_$_$_flora_mvs_rhs'(O,M,V)
#define FLORA_IFS_RHS(O,M,V)    '_$_$_flora_ifs_rhs'(O,M,V)
#define FLORA_IMVS_RHS(O,M,V)   '_$_$_flora_imvs_rhs'(O,M,V)


#define FLORA_DEFINED_FD(O,M)     '_$_$_flora_defined_fd'(O,M)
#define FLORA_DEFINED_MVD(O,M)    '_$_$_flora_defined_mvd'(O,M)
#define FLORA_DEFINED_IFD(O,M)    '_$_$_flora_defined_ifd'(O,M)
#define FLORA_DEFINED_IMVD(O,M)   '_$_$_flora_defined_imvd'(O,M)


#define FLORA_OVERWRITTEN_FD(O,C,M)  '_$_$_flora_overwritten_fd'(O,C,M)
#define FLORA_OVERWRITTEN_MVD(O,C,M)  '_$_$_flora_overwritten_mvd'(O,C,M)
#define FLORA_OVERWRITTEN_IFD(O,C,M)  '_$_$_flora_overwritten_ifd'(O,C,M)
#define FLORA_OVERWRITTEN_IMVD(O,C,M)  '_$_$_flora_overwritten_imvd'(O,C,M)


#define FLORA_CONFLICT_FD(C,S,M)    '_$_$_flora_conflict_fd'(C,S,M)
#define FLORA_CONFLICT_MVD(C,S,M)   '_$_$_flora_conflict_mvd'(C,S,M)
#define FLORA_CONFLICT_IFD(C,S,M)   '_$_$_flora_conflict_ifd'(C,S,M)
#define FLORA_CONFLICT_IMVD(C,S,M)  '_$_$_flora_conflict_imvd'(C,S,M)


#define FLORA_EXISTS(O)	       '_$_$_flora_exists'(O)
#define FLORA_EQL(O1,O2)       '_$_$_flora_eql'(O1,O2)
#define FLORA_METHEQL(M1,M2)   '_$_$_flora_metheql'(M1,M2)
#define FLORA_ARGEQL(A1,A2,V)   '_$_$_flora_argeql'(A1,A2,V)
