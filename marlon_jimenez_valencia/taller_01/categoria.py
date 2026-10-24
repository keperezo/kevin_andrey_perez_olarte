"""
categoria.py — Reglas que delimitan la categoría del taller.

Categoría registrada en la planilla:
    "Medicamentos antiinfecciosos de uso sistémico: antibióticos, antivirales,
     antimicóticos sistémicos y antiparasitarios. Excluye tratamientos locales
     asignados a piel, ojos u oídos."

Ninguno de los tres comercios tiene una sección que coincida exactamente con
esa delimitación (La Rebaja y Cafam no separan los medicamentos con fórmula
por grupo terapéutico, y la sección "Antiinfecciosos" de Locatel mezcla
productos que no son antiinfecciosos). Por eso el scraper recorre TODA la
sección de medicamentos de cada comercio y la categoría se define aquí, con
reglas explícitas basadas en el principio activo y la vía de administración.

Referencia usada para agrupar: clasificación ATC de la OMS
    J01 antibacterianos, J02 antimicóticos, J04 antimicobacterianos,
    J05 antivirales (uso sistémico), P01 antiprotozoarios, P02 antihelmínticos,
    D01BA antimicóticos de uso sistémico (terbinafina oral, griseofulvina).

Las decisiones de delimitación que no salen directo del código ATC están marcadas
con "DECISIÓN" más abajo.
"""
from __future__ import annotations

import re
import unicodedata

# ---------------------------------------------------------------------------
# 1. Principios activos que SÍ pertenecen a la categoría
#    clave: texto normalizado (sin tildes, minúsculas) que se busca
#    valor: (nombre estándar, subcategoría)
# ---------------------------------------------------------------------------
ANTIBIOTICO = "Antibiótico"
ANTIVIRAL = "Antiviral"
ANTIMICOTICO = "Antimicótico sistémico"
ANTIPARASITARIO = "Antiparasitario"

PRINCIPIOS_SISTEMICOS: dict[str, tuple[str, str]] = {
    # --- Antibacterianos (J01) y antimicobacterianos (J04) ---
    "amoxicilina": ("AMOXICILINA", ANTIBIOTICO),
    "ampicilina": ("AMPICILINA", ANTIBIOTICO),
    "sultamicilina": ("SULTAMICILINA", ANTIBIOTICO),
    "dicloxacilina": ("DICLOXACILINA", ANTIBIOTICO),
    "oxacilina": ("OXACILINA", ANTIBIOTICO),
    "penicilina": ("PENICILINA", ANTIBIOTICO),
    "fenoximetilpenicilina": ("PENICILINA V", ANTIBIOTICO),
    "piperacilina": ("PIPERACILINA", ANTIBIOTICO),
    # en las cefalosporinas buscamos la raíz: así sirve igual "cefpodoxima" que
    # "cefpodoxime" (Locatel a veces escribe el nombre en inglés)
    "cefalexin": ("CEFALEXINA", ANTIBIOTICO),
    "cefadroxil": ("CEFADROXILO", ANTIBIOTICO),
    "cefradin": ("CEFRADINA", ANTIBIOTICO),
    "cefalotin": ("CEFALOTINA", ANTIBIOTICO),
    "cefaclor": ("CEFACLOR", ANTIBIOTICO),
    "cefuroxim": ("CEFUROXIMA", ANTIBIOTICO),
    "cefixim": ("CEFIXIMA", ANTIBIOTICO),
    "cefpodox": ("CEFPODOXIMA", ANTIBIOTICO),
    "cefdinir": ("CEFDINIR", ANTIBIOTICO),
    "cefazolin": ("CEFAZOLINA", ANTIBIOTICO),
    "ceftriaxon": ("CEFTRIAXONA", ANTIBIOTICO),
    "cefotaxim": ("CEFOTAXIMA", ANTIBIOTICO),
    "ceftazidim": ("CEFTAZIDIMA", ANTIBIOTICO),
    "cefepim": ("CEFEPIME", ANTIBIOTICO),
    "azitromicina": ("AZITROMICINA", ANTIBIOTICO),
    "claritromicina": ("CLARITROMICINA", ANTIBIOTICO),
    "eritromicina": ("ERITROMICINA", ANTIBIOTICO),
    "roxitromicina": ("ROXITROMICINA", ANTIBIOTICO),
    "clindamicina": ("CLINDAMICINA", ANTIBIOTICO),
    "lincomicina": ("LINCOMICINA", ANTIBIOTICO),
    "doxiciclina": ("DOXICICLINA", ANTIBIOTICO),
    "doxicicilina": ("DOXICICLINA", ANTIBIOTICO),       # errata de Locatel (Oracea)
    "minociclina": ("MINOCICLINA", ANTIBIOTICO),
    "tetraciclina": ("TETRACICLINA", ANTIBIOTICO),
    "oxitetraciclina": ("OXITETRACICLINA", ANTIBIOTICO),
    "limeciclina": ("LIMECICLINA", ANTIBIOTICO),
    "ciprofloxacin": ("CIPROFLOXACINA", ANTIBIOTICO),   # ciprofloxacina / ciprofloxacino
    "levofloxacin": ("LEVOFLOXACINA", ANTIBIOTICO),
    "moxifloxacin": ("MOXIFLOXACINA", ANTIBIOTICO),
    "norfloxacin": ("NORFLOXACINA", ANTIBIOTICO),
    "ofloxacin": ("OFLOXACINA", ANTIBIOTICO),
    "gatifloxacin": ("GATIFLOXACINA", ANTIBIOTICO),
    "pipemidico": ("ACIDO PIPEMIDICO", ANTIBIOTICO),     # J01MB04 (Mixato)
    "nalidixico": ("ACIDO NALIDIXICO", ANTIBIOTICO),
    "nitrofurantoina": ("NITROFURANTOINA", ANTIBIOTICO),
    "fosfomicina": ("FOSFOMICINA", ANTIBIOTICO),
    "trimetoprim": ("TRIMETOPRIM", ANTIBIOTICO),
    "sulfametoxazol": ("SULFAMETOXAZOL", ANTIBIOTICO),
    "gentamicina": ("GENTAMICINA", ANTIBIOTICO),
    "amikacina": ("AMIKACINA", ANTIBIOTICO),
    "estreptomicina": ("ESTREPTOMICINA", ANTIBIOTICO),
    "vancomicina": ("VANCOMICINA", ANTIBIOTICO),
    "linezolid": ("LINEZOLID", ANTIBIOTICO),
    "cloranfenicol": ("CLORANFENICOL", ANTIBIOTICO),   # J01BA01 (las gotas para ojos quedan por vía local)
    "tiamfenicol": ("TIAMFENICOL", ANTIBIOTICO),
    "teicoplanina": ("TEICOPLANINA", ANTIBIOTICO),
    "tigeciclina": ("TIGECICLINA", ANTIBIOTICO),
    "meropenem": ("MEROPENEM", ANTIBIOTICO),
    "ertapenem": ("ERTAPENEM", ANTIBIOTICO),
    "imipenem": ("IMIPENEM", ANTIBIOTICO),
    "rifampicina": ("RIFAMPICINA", ANTIBIOTICO),
    "isoniazida": ("ISONIAZIDA", ANTIBIOTICO),
    "pirazinamida": ("PIRAZINAMIDA", ANTIBIOTICO),
    "etambutol": ("ETAMBUTOL", ANTIBIOTICO),
    "dapsona": ("DAPSONA", ANTIBIOTICO),                 # J04BA02 (oral); el gel queda por "gel"
    # --- Antimicóticos de uso sistémico (J02 y D01BA) ---
    "fluconazol": ("FLUCONAZOL", ANTIMICOTICO),
    "itraconazol": ("ITRACONAZOL", ANTIMICOTICO),
    "ketoconazol": ("KETOCONAZOL", ANTIMICOTICO),
    "voriconazol": ("VORICONAZOL", ANTIMICOTICO),
    "posaconazol": ("POSACONAZOL", ANTIMICOTICO),
    "terbinafina": ("TERBINAFINA", ANTIMICOTICO),
    "griseofulvina": ("GRISEOFULVINA", ANTIMICOTICO),
    "anfotericina": ("ANFOTERICINA B", ANTIMICOTICO),
    "caspofungina": ("CASPOFUNGINA", ANTIMICOTICO),
    "anidulafungina": ("ANIDULAFUNGINA", ANTIMICOTICO),
    "micafungina": ("MICAFUNGINA", ANTIMICOTICO),
    # --- Antivirales (J05) ---
    "aciclovir": ("ACICLOVIR", ANTIVIRAL),
    "valaciclovir": ("VALACICLOVIR", ANTIVIRAL),
    "famciclovir": ("FAMCICLOVIR", ANTIVIRAL),
    "ganciclovir": ("GANCICLOVIR", ANTIVIRAL),
    "valganciclovir": ("VALGANCICLOVIR", ANTIVIRAL),
    "oseltamivir": ("OSELTAMIVIR", ANTIVIRAL),
    "inosina": ("INOSINA PRANOBEX", ANTIVIRAL),
    "metisoprinol": ("INOSINA PRANOBEX", ANTIVIRAL),
    "methisoprinol": ("INOSINA PRANOBEX", ANTIVIRAL),  # así lo escribe Locatel (Pranosina)
    "tenofovir": ("TENOFOVIR", ANTIVIRAL),
    "lamivudina": ("LAMIVUDINA", ANTIVIRAL),
    "zidovudina": ("ZIDOVUDINA", ANTIVIRAL),
    "emtricitabina": ("EMTRICITABINA", ANTIVIRAL),
    "efavirenz": ("EFAVIRENZ", ANTIVIRAL),
    "dolutegravir": ("DOLUTEGRAVIR", ANTIVIRAL),
    "raltegravir": ("RALTEGRAVIR", ANTIVIRAL),
    "realtegravir": ("RALTEGRAVIR", ANTIVIRAL),        # errata de Locatel (Isentress)
    "atazanavir": ("ATAZANAVIR", ANTIVIRAL),
    "darunavir": ("DARUNAVIR", ANTIVIRAL),
    "lopinavir": ("LOPINAVIR", ANTIVIRAL),
    "ritonavir": ("RITONAVIR", ANTIVIRAL),
    "nevirapina": ("NEVIRAPINA", ANTIVIRAL),
    "rilpivirina": ("RILPIVIRINA", ANTIVIRAL),
    "bictegravir": ("BICTEGRAVIR", ANTIVIRAL),
    "abacavir": ("ABACAVIR", ANTIVIRAL),
    "entecavir": ("ENTECAVIR", ANTIVIRAL),
    "sofosbuvir": ("SOFOSBUVIR", ANTIVIRAL),
    "ribavirina": ("RIBAVIRINA", ANTIVIRAL),
    "nirmatrelvir": ("NIRMATRELVIR", ANTIVIRAL),
    # --- Antiparasitarios (P01 antiprotozoarios, P02 antihelmínticos) ---
    "metronidazol": ("METRONIDAZOL", ANTIPARASITARIO),
    "tinidazol": ("TINIDAZOL", ANTIPARASITARIO),
    "secnidazol": ("SECNIDAZOL", ANTIPARASITARIO),
    "ornidazol": ("ORNIDAZOL", ANTIPARASITARIO),
    "nitazoxanida": ("NITAZOXANIDA", ANTIPARASITARIO),
    "teclozan": ("TECLOZAN", ANTIPARASITARIO),
    "etofamida": ("ETOFAMIDA", ANTIPARASITARIO),
    "albendazol": ("ALBENDAZOL", ANTIPARASITARIO),
    "mebendazol": ("MEBENDAZOL", ANTIPARASITARIO),
    "flubendazol": ("FLUBENDAZOL", ANTIPARASITARIO),
    "ivermectina": ("IVERMECTINA", ANTIPARASITARIO),
    "praziquantel": ("PRAZIQUANTEL", ANTIPARASITARIO),
    "pirantel": ("PIRANTEL", ANTIPARASITARIO),
    "oxantel": ("OXANTEL", ANTIPARASITARIO),
    "niclosamida": ("NICLOSAMIDA", ANTIPARASITARIO),
    "levamisol": ("LEVAMISOL", ANTIPARASITARIO),
    "piperazina": ("PIPERAZINA", ANTIPARASITARIO),
    "benznidazol": ("BENZNIDAZOL", ANTIPARASITARIO),
    "nifurtimox": ("NIFURTIMOX", ANTIPARASITARIO),
    "miltefosina": ("MILTEFOSINA", ANTIPARASITARIO),
    # DECISIÓN: antipalúdicos (ATC P01B). La hidroxicloroquina se usa sobre todo
    # para lupus y artritis, pero su código ATC es antiparasitario.
    "cloroquina": ("CLOROQUINA", ANTIPARASITARIO),
    "hidroxicloroquina": ("HIDROXICLOROQUINA", ANTIPARASITARIO),
    "primaquina": ("PRIMAQUINA", ANTIPARASITARIO),
    "mefloquina": ("MEFLOQUINA", ANTIPARASITARIO),
    "artemeter": ("ARTEMETER", ANTIPARASITARIO),
    "atovacuona": ("ATOVACUONA", ANTIPARASITARIO),
}

# Marcas que Locatel publica a veces sin principio activo (ni en la ficha ni en la
# descripción). Las revisamos a mano el 9 de octubre de 2026 con la ficha
# "Principio activo" que publican La Rebaja o Cafam para la misma marca.
# El orden importa: "sulamp duo" va antes que "sulamp".
MARCAS_SISTEMICAS: dict[str, str] = {
    "zinnat": "cefuroxima",
    "curam": "amoxicilina + acido clavulanico",
    "enitrax": "itraconazol",
    "itrol": "itraconazol",
    "albisec": "itraconazol + secnidazol",
    "pranosina": "metisoprinol",
    "helmintrel": "pirantel + oxantel",
    "sulamp duo": "amoxicilina + sulbactam",
    "sulamp": "sultamicilina",
}

# Nombres que sobran cuando aparecen juntos: la sultamicilina ES ampicilina con
# sulbactam, y la limeciclina es una tetraciclina (La Rebaja las publica juntas).
REDUNDANTES = {"SULTAMICILINA": {"AMPICILINA", "SULBACTAM"}, "LIMECICLINA": {"TETRACICLINA"}}

# Sustancias que acompañan a un antiinfeccioso en combinaciones (no bastan solas).
ACOMPANANTES: dict[str, str] = {
    "clavulan": "ACIDO CLAVULANICO",
    "sulbactam": "SULBACTAM",
    "tazobactam": "TAZOBACTAM",
    "nifuroxazida": "NIFUROXAZIDA",
}

# ---------------------------------------------------------------------------
# 2. Antiinfecciosos que NO son de uso sistémico (se registran como excluidos)
# ---------------------------------------------------------------------------
# DECISIÓN: la nistatina, la rifaximina y la nifuroxazida actúan en el intestino
# o la mucosa y casi no se absorben (ATC A07AA / A07AX, no J). Se excluyen.
PRINCIPIOS_SOLO_LOCALES: dict[str, str] = {
    "nistatina": "NISTATINA",
    "rifaximina": "RIFAXIMINA",
    # DECISIÓN: la furazolidona se toma oral, pero actúa en el intestino (se vende
    # para diarrea y amebiasis; La Rebaja la pone en "sistema digestivo") y su
    # código ATC es G01AX06, no J ni P. Va fuera, igual que la nifuroxazida.
    "furazolidona": "FURAZOLIDONA",
    "clotrimazol": "CLOTRIMAZOL",
    "miconazol": "MICONAZOL",
    "tioconazol": "TIOCONAZOL",
    "terconazol": "TERCONAZOL",
    "econazol": "ECONAZOL",
    "sertaconazol": "SERTACONAZOL",
    "isoconazol": "ISOCONAZOL",
    "ciclopirox": "CICLOPIROX",
    "amorolfina": "AMOROLFINA",
    "neomicina": "NEOMICINA",
    "mupirocina": "MUPIROCINA",
    "fusidico": "ACIDO FUSIDICO",
    "bacitracina": "BACITRACINA",
    "polimixina": "POLIMIXINA B",
    "tobramicina": "TOBRAMICINA",
    "permetrina": "PERMETRINA",
    "sulfadiazina de plata": "SULFADIAZINA DE PLATA",
    "plata sulfadiazina": "SULFADIAZINA DE PLATA",     # así lo escribe Locatel
    "rifamicina": "RIFAMICINA",                        # spray para heridas
    "fenticonazol": "FENTICONAZOL",                    # óvulos y crema vaginal
    "teconazol": "TERCONAZOL",                         # errata de Locatel (Decadron T)
}

# ---------------------------------------------------------------------------
# 3. Palabras que indican tratamiento LOCAL (vía no sistémica)
# ---------------------------------------------------------------------------
LOCAL_PIEL = [r"crema", r"unguento", r"pomada", r"\bgel\b", r"locion", r"shampoo",
              r"champu", r"topic", r"dermic", r"cutane", r"esmalte", r"laca",
              r"espuma", r"jabon", r"polvo topico", r"talco", r"spray", r"\btubo\b",
              r"emulsion"]
LOCAL_OJOS = [r"oftalm", r"ofteno", r"colirio", r"ocular"]
LOCAL_OIDOS = [r"\botic", r"otosec", r"\botico"]
# DECISIÓN: la planilla solo nombra piel, ojos y oídos como ejemplos de uso local.
# Los óvulos y cremas vaginales tampoco son de uso sistémico (ATC G01), así que
# también se excluyen. Para incluirlos bastaría con vaciar esta lista.
LOCAL_VAGINAL = [r"ovulo", r"vaginal", r"intravaginal"]
LOCAL_OTROS = [r"nasal", r"enjuague", r"bucal"]

# Las vías se comparan por el comienzo de la palabra: así "TOPICA" y "TOPICO", o
# "INTRAVENOSA" e "INTRAVENOSO", caen en el mismo grupo.
VIAS_SISTEMICAS = ("ORAL", "PARENTER", "INTRAMUSC", "INTRAVEN", "SUBCUT", "INYECT", "SUBLINGU")
VIAS_LOCALES = ("TOPIC", "OFTALM", "OTIC", "VAGIN", "INTRAVAGIN", "NASAL", "CUTANE", "DERMIC",
                "BUCAL", "RECTAL", "OCULAR")

# Ningún antibiótico oral o inyectable viene mezclado con un corticoide o con
# adapaleno: esas mezclas son gotas para ojos u oídos, o cremas. (Aceoto Plus,
# Vigadexa, Fixamicin Dexacipro, Iloticina Duo...)
ACOMPANANTES_LOCALES = [r"dexametasona", r"fluocinolona", r"hidrocortisona", r"betametasona",
                        r"prednisolona", r"fluorometolona", r"loteprednol", r"clobetasol",
                        r"adapalen"]

# Marcas de uso local que el comercio publica sin decir la vía en ninguna parte
# (las revisamos a mano el 3 de octubre de 2026): Iloticina es eritromicina para
# el acné y Clindamizol son óvulos de clindamicina con ketoconazol.
MARCAS_LOCALES = [r"iloticina", r"clindamizol"]

# Secciones del comercio que solo tienen productos para ojos, oídos o piel.
# No se aplica a tabletas, cápsulas, inyectables o suspensiones que el comercio
# haya puesto ahí (p. ej. doxiciclina para el acné en "dermatológicos").
SECCIONES_LOCALES = [r"organos.sensoriales", r"dermatolog", r"cuidado de los ojos", r"oftalm"]
FORMAS_SISTEMICAS = (r"tableta|\btab\b|capsula|\bcap\b|comprimido|gragea|inyect|ampoll|\bvial"
                     r"|suspension|\bsusp\b|jarabe|\boral\b|\bsobres?\b")

# La Rebaja vende gotas para ojos y oídos sin decir la vía (p. ej. "VIGAMOX,
# FRASCO X 5 ML"). Estos antibióticos solo vienen en frascos de 10 mL o menos
# cuando son gotas; como suspensión oral traen 15 mL o más.
GOTAS_POR_TAMANO = {"CIPROFLOXACINA", "LEVOFLOXACINA", "MOXIFLOXACINA", "OFLOXACINA",
                    "GATIFLOXACINA", "AZITROMICINA", "TOBRAMICINA", "CLORANFENICOL"}
ML_MAXIMO_GOTAS = 10

# Secciones de los comercios cuyo nombre sugiere la categoría (para documentar
# qué productos de esas secciones quedan fuera según la delimitación).
PATRON_SECCION_ANTIINFECCIOSA = re.compile(r"antiinfec|antibiot|antimic|antipar|antivir", re.I)


def normalizar(texto) -> str:
    """Minúsculas, sin tildes y con espacios simples (no modifica el original)."""
    if texto is None:
        return ""
    texto = str(texto)
    texto = unicodedata.normalize("NFD", texto)
    texto = "".join(c for c in texto if unicodedata.category(c) != "Mn")
    return re.sub(r"\s+", " ", texto.lower()).strip()


def _buscar(diccionario: dict, texto_norm: str) -> list:
    encontrados = []
    for clave, valor in diccionario.items():
        if re.search(r"(?<![a-z])" + re.escape(clave), texto_norm):
            if valor not in encontrados:
                encontrados.append(valor)
    return encontrados


def _legible(patron: str) -> str:
    return patron.replace(r"\b", "")


def detectar_via_local(texto_norm: str, via_declarada: str | None,
                       seccion: str | None = None, principios: list[str] = ()) -> str | None:
    """
    Devuelve el motivo si el producto es de uso local; None si parece sistémico.
    Las reglas van en este orden (la primera que se cumpla decide):
      1. vía local declarada por el comercio
      2. mezcla con corticoide o adapaleno
      3. marca de uso local revisada a mano
      4. vía sistémica declarada por el comercio -> sistémico
      5. palabras de uso local en el nombre o la presentación
      6. sección del comercio para ojos, oídos o piel (salvo formas orales o inyectables)
      7. gotas por tamaño: fluoroquinolona o azitromicina en frasco de 10 mL o menos
    """
    via = normalizar(via_declarada).upper()
    if via.startswith(VIAS_LOCALES):
        return f"vía declarada por el comercio: {via}"
    for p in ACOMPANANTES_LOCALES:
        if re.search(p, texto_norm):
            return f"mezcla con corticoide o retinoide (uso local): '{_legible(p)}'"
    for p in MARCAS_LOCALES:
        if re.search(p, texto_norm):
            return f"marca de uso local (revisada a mano): '{_legible(p)}'"
    if via.startswith(VIAS_SISTEMICAS):
        return None
    for grupo, patrones in (("piel", LOCAL_PIEL), ("ojos", LOCAL_OJOS),
                            ("oídos", LOCAL_OIDOS), ("vaginal", LOCAL_VAGINAL),
                            ("otra vía local", LOCAL_OTROS)):
        for p in patrones:
            if re.search(p, texto_norm):
                # "gotas" orales (p. ej. ivermectina 0,6 %) no se excluyen: solo
                # se excluye si aparece una palabra de uso local.
                return f"uso local ({grupo}): '{_legible(p)}'"
    seccion_norm = normalizar(seccion)
    for p in SECCIONES_LOCALES:
        if re.search(p, seccion_norm) and not re.search(FORMAS_SISTEMICAS, texto_norm):
            return f"sección del comercio para uso local: '{seccion}'"
    if GOTAS_POR_TAMANO.intersection(principios) and not re.search(FORMAS_SISTEMICAS, texto_norm):
        m = re.search(r"\bx\s*(\d+(?:[.,]\d+)?)\s*ml\b", texto_norm)
        if m and float(m.group(1).replace(",", ".")) <= ML_MAXIMO_GOTAS:
            return f"gotas para ojos u oídos (frasco de {m.group(1)} mL)"
    return None


def clasificar(principio_activo: str | None, nombre: str | None,
               presentacion: str | None = None, via: str | None = None,
               seccion_comercio: str | None = None) -> dict:
    """
    Decide si un anuncio pertenece a la categoría.

    Devuelve un diccionario con:
        incluido (bool), principio_activo_std, subcategoria, motivo_exclusion
    """
    pa_norm = normalizar(principio_activo)
    texto_norm = normalizar(" ".join(str(x) for x in (nombre, presentacion, principio_activo) if x))
    # Se busca primero en el principio activo declarado; si el comercio no lo
    # publica, se usa el nombre del producto.
    base = pa_norm if pa_norm else normalizar(nombre)

    sistemicos = _buscar(PRINCIPIOS_SISTEMICOS, base) or _buscar(PRINCIPIOS_SISTEMICOS, normalizar(nombre))
    locales = _buscar(PRINCIPIOS_SOLO_LOCALES, base)
    acompanantes = _buscar(ACOMPANANTES, base) or _buscar(ACOMPANANTES, normalizar(nombre))
    if not sistemicos and not locales:
        # último recurso: marcas revisadas a mano que no traen el principio activo
        for marca, principio in MARCAS_SISTEMICAS.items():
            if re.search(r"(?<![a-z])" + marca + r"(?![a-z])", normalizar(nombre)):
                sistemicos = _buscar(PRINCIPIOS_SISTEMICOS, principio)
                acompanantes = _buscar(ACOMPANANTES, principio)
                break

    resultado = {"incluido": False, "principio_activo_std": None,
                 "subcategoria": None, "motivo_exclusion": None}

    if not sistemicos:
        if locales:
            resultado["principio_activo_std"] = " + ".join(locales)
            resultado["motivo_exclusion"] = "antiinfeccioso de acción local o intestinal (no sistémico)"
        elif seccion_comercio and PATRON_SECCION_ANTIINFECCIOSA.search(seccion_comercio):
            resultado["motivo_exclusion"] = "está en una sección antiinfecciosa del comercio, pero su principio activo no es antiinfeccioso sistémico"
        else:
            resultado["motivo_exclusion"] = "no es antiinfeccioso"
        return resultado

    nombres = [n for n, _ in sistemicos]
    subcats = sorted({s for _, s in sistemicos})
    todos = set(nombres + acompanantes + locales)
    for principal, sobran in REDUNDANTES.items():
        if principal in todos:
            todos -= sobran
    resultado["principio_activo_std"] = " + ".join(sorted(todos))
    resultado["subcategoria"] = " / ".join(subcats)

    motivo_local = detectar_via_local(texto_norm, via, seccion_comercio, nombres)
    if motivo_local:
        resultado["motivo_exclusion"] = motivo_local
        return resultado

    resultado["incluido"] = True
    return resultado


def describir_reglas() -> str:
    """Resumen legible de las reglas (para mostrar en el documento)."""
    lineas = [
        f"- Principios activos sistémicos reconocidos: {len(PRINCIPIOS_SISTEMICOS)}",
        f"- Antiinfecciosos solo locales (se excluyen): {', '.join(sorted(set(PRINCIPIOS_SOLO_LOCALES.values())))}",
        f"- Palabras de uso local en piel: {', '.join(_legible(p) for p in LOCAL_PIEL)}",
        f"- Palabras de uso local en ojos: {', '.join(LOCAL_OJOS)}",
        f"- Palabras de uso local en oídos: {', '.join(_legible(p) for p in LOCAL_OIDOS)}",
        f"- Palabras de uso vaginal: {', '.join(LOCAL_VAGINAL)}",
        f"- Mezclas que solo existen en uso local: {', '.join(ACOMPANANTES_LOCALES)}",
        f"- Marcas de uso local revisadas a mano: {', '.join(MARCAS_LOCALES)}",
        f"- Marcas sin principio activo publicado (revisadas a mano): "
        f"{', '.join(f'{m} = {p}' for m, p in MARCAS_SISTEMICAS.items())}",
        f"- Secciones del comercio para ojos, oídos o piel: {', '.join(SECCIONES_LOCALES)}",
        f"- Gotas por tamaño (frasco de {ML_MAXIMO_GOTAS} mL o menos): "
        f"{', '.join(sorted(GOTAS_POR_TAMANO))}",
    ]
    return "\n".join(lineas)
