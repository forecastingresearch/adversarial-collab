from math import log10
import pdb

mult = 1

def KL(p, q):
    return p*mult * log10(p*mult / q*mult) + (mult - p*mult) * log10((mult - p*mult) / (mult - q*mult))

def log_voi(pu, puc, pc, punotc = None):
    if punotc == None:
        punotc = (pu - puc * pc) / (1 - pc)
    if pu == None:
        return None
    if pu == 0:
        return 0
    if puc == pu:
        return 0
    if punotc < 0:
        return None
    l_puc_pu = KL(puc, pu)
    l_punotc_pu = KL(punotc, pu)
    answer = (l_puc_pu * pc + l_punotc_pu * (1 - pc)) / mult
    return answer

# PU, PUC, PC
CQ30 = log_voi(1e-8, 1e-7, 1e-6)
JC50 = log_voi(1e-8, 4e-8, 1e-6)
LR70 = log_voi(1e-8, 1e-6, 1e-5)
CQ40 = log_voi(1e-8, 1e-7, 1e-8)
HN50 = log_voi(1e-8, 1e-8, 1e-1)

print(CQ30)
print(JC50)
print(LR70)
print(CQ40)
print(HN50)