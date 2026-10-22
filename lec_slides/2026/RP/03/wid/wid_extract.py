"""Stream the national members of the WID.world bulk zip (via curl byte ranges) and
keep only the rows needed for the world wealth distribution and top-1% shares.
python urllib/ssl segfaults on this box, so HTTP goes through curl subprocesses."""
import csv, io, re, subprocess, sys, zipfile

URL = "https://wid.world/bulk_download/wid_all_data.zip"
OUT = sys.argv[1]
PCT = {f"p{i}p{i + 1}" for i in range(100)}                    # 100 one-percent groups
KEEP = {"ahwealj992": PCT,                                      # avg net personal wealth, adults, equal split
        "shwealj992": {"p99p100"},                              # top 1% wealth share
        "npopuli992": {"p0p100"},                               # adult population (20+)
        "xlcusxi999": {"p0p100"},                               # market exchange rate, LCU per USD
        "anninci992": {"p0p100"}}                               # national income per adult


def curl(args):
    return subprocess.run(["curl", "-sfL", "--retry", "5"] + args + [URL], capture_output=True, check=True).stdout


class HttpFile(io.RawIOBase):
    def __init__(self):
        hdr = curl(["-I"]).decode().lower()
        self.size = int([l for l in hdr.splitlines() if l.startswith("content-length")][-1].split(":")[1])
        self.pos = 0
    def seekable(self): return True
    def readable(self): return True
    def tell(self): return self.pos
    def seek(self, off, whence=0):
        self.pos = off if whence == 0 else (self.pos + off if whence == 1 else self.size + off)
        return self.pos
    def readinto(self, b):
        n = min(len(b), self.size - self.pos)
        if n <= 0: return 0
        data = curl(["-r", f"{self.pos}-{self.pos + n - 1}"])
        b[:len(data)] = data
        self.pos += len(data)
        return len(data)


z = zipfile.ZipFile(io.BufferedReader(HttpFile(), buffer_size=8 << 20))
cc = sorted(m.group(1) for i in z.infolist() if (m := re.fullmatch(r"WID_data_([A-Z]{2})\.csv", i.filename)))
with open(f"{OUT}/wid_extract.csv", "w", newline="") as fo, open(f"{OUT}/wid_meta.csv", "w", newline="") as fm:
    wo, wm = csv.writer(fo), csv.writer(fm)
    for j, c in enumerate(cc):
        for kind, w in (("data", wo), ("metadata", wm)):
            name = f"WID_{kind}_{c}.csv"
            if name not in z.namelist(): continue
            with z.open(name) as src:
                r = csv.reader(io.TextIOWrapper(src, encoding="utf-8"), delimiter=";")
                head = next(r)
                if j == 0: w.writerow(head)
                vi = head.index("variable")
                pi = head.index("percentile") if "percentile" in head else None
                k = 0
                for row in r:
                    v = row[vi]
                    if v in KEEP and (pi is None or row[pi] in KEEP[v]):
                        w.writerow(row); k += 1
            if kind == "data": print(j + 1, c, k, "rows", flush=True)
