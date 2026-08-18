# Expert Elicitation & Statistical Platform

**SCTIMST · AMCHSS** — SHELF-style expert elicitation for HTA / clinical judgement.

This repository is a **Shiny** app (`app.R`). Publish **this folder** to the institute **Posit Connect** server. Do not publish the older React / FastAPI project.

Staff work at the app home page. Experts open a survey link of the form `/?study=<slug>`.

---

## Run on Posit Connect (institute)

This is the path for the manager / IT.

### What you need

1. A Posit Connect account (from institute IT).
2. RStudio Desktop, with the institute Connect server already added  
   (**Tools → Global Options → Publishing**).
3. The MongoDB Atlas connection string — sent **separately**, never stored in this repo.
4. IT must whitelist the **Posit server outbound IP** in Atlas → Network Access.

### Publish from RStudio

1. Clone this repository and open `elicitation.Rproj` in RStudio.
2. Open `app.R`.
3. Click the blue **Publish** icon → **Posit Connect**.
4. Choose the institute Connect server.
5. After the app appears, open that content → **Vars** and set:

| Variable | Value |
|----------|--------|
| `MONGODB_URI` | Atlas URI (from the developer, not GitHub) |
| `MONGODB_DB` | `expert_elicitation_shiny` |
| `AUTH_DEV_MODE` | `false` |
| `APP_ROLE` | `both` |
| `SURVEY_PUBLIC_URL` | the Posit URL of **this same app** (see below) |

6. Restart the content after saving Vars.
7. **Access:** require login for staff. For expert survey links, either invite named Connect users, or allow Anyone and keep the invited-email gate in the app.

`manifest.json` is included so Connect treats this as a Shiny app.

### What `SURVEY_PUBLIC_URL` is

After the first publish, Posit gives the app a public address, for example:

```text
https://posit.YOUR-INSTITUTE.edu/content/12ab34cd/
```

Paste **that address** (no `?study=...`) into `SURVEY_PUBLIC_URL`.

The app then builds invite links such as:

```text
https://posit.YOUR-INSTITUTE.edu/content/12ab34cd/?study=hta-drug-a-vs-b
```

Until this variable is set, invite links still point at `http://127.0.0.1:3938`, which only works on a local laptop.

### First check after publish

1. Open the Posit URL while signed in (facilitator / admin).
2. Click **Seed HTA demo**.
3. Open the demo study and copy the survey URL.
4. Open that URL (new window). Enter `priya.nair@hospital.org`.
5. Dummy expert values are already seeded so you can open **Responses / SHELF** and click **Run SHELF**.

### Logs

Connect → this content → **Logs**. Dev role-picker login is off when `AUTH_DEV_MODE=false`.

### Publish with an API key (optional)

Ask IT for the Connect URL. Create a key: Connect → your name → **API Keys**. From this folder:

```powershell
$env:CONNECT_SERVER="https://posit.YOUR-INSTITUTE.edu"
$env:CONNECT_API_KEY="paste-key-here"
& "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" deploy/connect.R
```

Never commit the API key.

---

## Local run (developer laptop)

Use this only to try the app before Posit. Experts on the institute network will use the Posit URL, not `127.0.0.1`.

### 1. Install R (4.1 or newer) and RStudio

https://cloud.r-project.org/ · https://posit.co/download/rstudio-desktop/

### 2. Configure MongoDB

Copy `.Renviron.example` → `.Renviron` in this folder. Do not commit `.Renviron`.

**Atlas (usual):**

```
MONGODB_URI=mongodb+srv://USER:PASSWORD@YOUR-CLUSTER.mongodb.net
MONGODB_DB=expert_elicitation_shiny
AUTH_DEV_MODE=true
APP_ROLE=both
SURVEY_PUBLIC_URL=http://127.0.0.1:3938
```

Use database name `expert_elicitation_shiny` so the older React app is not overwritten. Whitelist your laptop IP in Atlas.

**Or local MongoDB:** keep `MONGODB_URI=mongodb://127.0.0.1:27017` and run mongod / Docker.

### 3. Start

RStudio: open `elicitation.Rproj` → open `run_local.R` → Source.

Or:

```r
setwd("D:/Projects/sct/rwebapp")
source("run_local.R")
```

Browser: **http://127.0.0.1:3938**

Sign in as Facilitator (`facilitator@sctimst.ac.in`) → **Seed HTA demo**.

Local errors also append to `logs/shiny.log` (gitignored).

---

## R packages

Installed automatically on first local run / by Connect from `manifest.json`:

`shiny`, `htmltools`, `mongolite`, `jsonlite`, `ggplot2`, `plotly`, `SHELF`

---

## What this app does

- Role login (admin, facilitator, researcher, expert, student)
- Create a SHELF survey (chips-and-bins + P10 / P50 / P90)
- Invite experts by email
- Save judgements and run `SHELF::fitdist` with a pooled density chart
- Seeded HTA demo study `hta-drug-a-vs-b` with dummy expert values for testing SHELF

---

## Do not

- Commit `.Renviron`, API keys, or Atlas passwords
- Point `MONGODB_DB` at the live React database while both apps are in use
- Publish any folder except this Shiny repository
