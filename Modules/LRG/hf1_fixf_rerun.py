import sys, os, time, json
sys.path.insert(0, os.path.expanduser('~/mnt/PV_PL/Modules/LRG'))
import mesh_offset, seal3d
flights = ["2024_H0063_OPP/20240918_123329/20240918_123329_M3T_20M", "2024_H0063_OPP/20240918_125737/20240918_125737_M3T_20M",
           "2024_H0233_OPP/20240917_094533/20240917_094533_MINI3PRO_20M", "2024_H0060_OPP/20240918_095054/20240918_095054_MINI3PRO_20M",
           "2024_H0055_OPP/20240917_091131/20240917_091131_MINI3PRO_20M", "2023_H0052A_OPP/20230518_104633/20230518_104633_MINI2_20m",
           "2023_H0233_OPP/20230718_111424/20230718_111424_MINI2_20m"]
budget = float(sys.argv[1]) if len(sys.argv) > 1 else 140
t_all = time.time()
for fl in flights:
    bs = os.path.basename(fl); v = fl + '_fixf'; pred = os.path.join(v, 'Predict')
    flag = os.path.join(pred, '_reg_v2_done')
    if os.path.exists(flag):
        continue
    if time.time() - t_all > budget:
        print('time budget reached'); break
    if not os.path.exists(os.path.join(pred, '_reg_v2_started')):
        for f in os.listdir(pred):                               # stale cache/registration of the first run -> aside
            if f.startswith('_seal3d_cache') or f.startswith('_seal3d_reg'):
                os.rename(os.path.join(pred, f), os.path.join(pred, 'old_' + f + '.bak'))
        offs = mesh_offset.tile_offsets(os.path.join(fl, bs + '_Model'), os.path.join(v, bs + '_fixf_Model'), verbose=False)
        json.dump(offs, open(os.path.join(pred, '_reg_v2_started'), 'w'))
        print(bs, 'offsets', {k: [round(x, 2) for x in val] for k, val in offs.items()})
    offs = json.load(open(os.path.join(pred, '_reg_v2_started')))
    st = seal3d.run_flight(os.path.abspath(v), res=0.01, plots=False, time_budget=max(20, budget - (time.time() - t_all)), reg_init=offs, reg_search=1.5, verbose=False)
    print(bs, 'HF1', st, '%.0fs' % (time.time() - t_all))
    if st == 'done':
        open(flag, 'w').write('ok')
print('elapsed %.0f s' % (time.time() - t_all))
