"""Vista HTML del Quarto mediante Jupyter y nbconvert. Alternativa de exportación."""
from pathlib import Path
import re,nbformat
from nbclient import NotebookClient
from nbconvert import HTMLExporter
root=Path(__file__).resolve().parent
text=(root/'taller_1.qmd').read_text(encoding='utf8')
text=re.sub(r'^---\n.*?\n---\n','',text,count=1,flags=re.S)
parts=re.split(r'```\{python\}\s*\n(.*?)\n```',text,flags=re.S)
cells=[nbformat.v4.new_markdown_cell('# Comparador de precios de higiene corporal\n\n**Thomas Matallana** · Taller 1 · Minería de Datos · 2016325\n\nVista HTML de los bloques del Quarto ejecutados con Jupyter. La exportación HTML se hizo con nbconvert porque el renderizador Quarto encontró restricciones del entorno.')]
for i,chunk in enumerate(parts):
 cells.append(nbformat.v4.new_code_cell(chunk) if i%2 else nbformat.v4.new_markdown_cell(chunk))
nb=nbformat.v4.new_notebook(cells=cells,metadata={'kernelspec':{'name':'python3','display_name':'Python 3','language':'python'},'language_info':{'name':'python'}})
client=NotebookClient(nb,timeout=180,kernel_name='python3',resources={'metadata':{'path':str(root)}})
client.execute()

exporter=HTMLExporter(template_name='lab'); exporter.exclude_input_prompt=True; exporter.exclude_output_prompt=True
body,resources=exporter.from_notebook_node(nb)
body=body.replace('<title>Notebook</title>','<title>Higiene corporal · Thomas Matallana</title>')
body=body.replace('</head>','<style>.jp-Notebook{max-width:1180px;margin:auto;padding:35px}h1,h2{color:#164966}.jp-RenderedHTMLCommon table{font-size:13px} .jp-OutputArea-output{overflow-x:auto}</style></head>')
from bs4 import BeautifulSoup
soup = BeautifulSoup(body, 'html.parser')
for node in soup.select('.jp-CodeCell .jp-Cell-inputWrapper'):
    details = soup.new_tag('details')
    summary = soup.new_tag('summary')
    summary.string = 'Ver código Python'
    node.wrap(details)
    details.insert(0, summary)
(root/'taller_1.html').write_text(str(soup), encoding='utf8')
print('BLOQUES EJECUTADOS',sum(c.cell_type=='code' for c in nb.cells));print('HTML',len(body))
