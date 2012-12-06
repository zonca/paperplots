from setup_matplotlib import *
import matplotlib.pyplot as plt
import sys
sys.path.append("../cosmoslik")
# requires cosmoslik, contact Andrea Zonca and Marius Millea to get it
import cosmoslik

fold = "/global/project/projectdirs/planck/user/marius/uspype/runs/dx9/143.217.mask4/postchains/"
fold = "data/"
filename = "lcdm6.143c5.4fg.lmax1300.ifixed.beamfixed.tauprior.bs.ptm.chain"

c=cosmoslik.chains.load_chain(fold+filename)

for v in c.itervalues():
    v *= np.random.rand(1)

fig = cosmoslik.chains.likegrid(c, params=[p for p in c.keys() if not (p.startswith("egfs") or p=="weight")], size=cm2inch(18))
plt.show()
plt.savefig("latex/params.pdf")
