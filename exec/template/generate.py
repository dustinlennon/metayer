from jinja2 import Environment, PackageLoader, select_autoescape, FileSystemLoader

from yaml import load, dump
try:
  from yaml import CLoader as Loader, CDumper as Dumper
except ImportError:
  from yaml import Loader, Dumper

from io import StringIO
from glob import glob

env = Environment(
    loader=FileSystemLoader("./exec/template/templates"),
    autoescape=select_autoescape()
)

with StringIO() as sio:
  wrap_files = glob("./exec/template/wraps_*.yaml")
  for wf in wrap_files:
    with open(wf) as f:
      sio.write(f.read())

  sio.seek(0)
  data = load(sio, Loader=Loader)

template = env.get_template("cli_wrapped.R.j2")

for k,v in data.items():  
  with open(v['output'], "w") as f:
    txt = template.render(pkg = k, name_levels = v['exports'])
    f.write(txt)


