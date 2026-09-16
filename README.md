# Expert Elicitation Platform

**SCTIMST · AMCHSS** (Achutha Menon Centre for Health Science Studies) — Sheffield Elicitation Framework (SHELF v4) platform for Health Technology Assessment (HTA) and clinical decision support.

This repository is a unified **R Shiny** web application (`app.R`) backed by **MongoDB** and the native **SHELF** statistical package.

---

## Core Technology Stack

* **Application Framework:** R Shiny (`fluidPage` with dynamic routing via `app.R` and `R/app_server.R`).
* **Persistence Tier:** MongoDB via `mongolite` (database: `expert_elicitation_shiny`), preserving nested JSON structures via cursor-based iteration (`iterate()`).
* **Analytical Engine:** In-process CRAN `SHELF` package (`SHELF::fitdist`, linear opinion pools, and median pools).
* **Visualizations:** `ggplot2` density curves rendered interactively via `plotly` (`ggplotly`).
* **Hosting Options:** Local developer runner (`run_local.R`), Docker containerization (e.g., Render free tier), or enterprise deployment on **Posit Connect** (`manifest.json`).

---

## Application Modes & User Roles

Controlled by the `APP_ROLE` environment variable (`staff`, `survey`, or `both`):

* **Admin:** System oversight, directory management, and administrative provisioning.
* **Facilitator / Study Manager:** Creates case studies, configures Quantities of Interest (QoIs), invites experts, manages round transitions, and executes SHELF analyses.
* **Expert:** Completes secure, tokenized surveys (Chips-N-Bins or percentiles), submits written rationales, and participates in Round 2 peer review.
* **Researcher / Student:** Role-based access to view assigned studies and historical consensus models.

---

## Local Run (Developer Laptop)

### 1. Requirements
* R (version 4.1 or newer) and RStudio / VS Code.
* Local `mongod` instance or connection URI to a **MongoDB Atlas** cluster.

### 2. Configuration
Copy `.Renviron.example` to `.Renviron` in the project root (never commit `.Renviron`):

```env
MONGODB_URI=mongodb+srv://USER:PASSWORD@cluster.mongodb.net/?retryWrites=true&w=majority
MONGODB_DB=expert_elicitation_shiny
AUTH_DEV_MODE=true
APP_ROLE=both
SURVEY_PUBLIC_URL=[http://127.0.0.1:3938](http://127.0.0.1:3938)
SESSION_TIMEOUT_MINUTES=30
```

### Render + Auth0/OIDC

The Render deployment uses the Docker configuration and reads the assigned `PORT`.
Copy the values from `render.yaml` into Render, then configure the OIDC application
with:

* **Application type:** Regular Web Application
* **Allowed callback URL:** exactly `OIDC_REDIRECT_URI`
* **Allowed logout URL:** exactly `SURVEY_PUBLIC_URL`
* **Allowed web origins:** exactly `SURVEY_PUBLIC_URL`

Set `OIDC_ISSUER` to the issuer URL (for Auth0, for example
`https://your-tenant.us.auth0.com`), and generate a long random
`OIDC_STATE_SECRET`. Set `AUTH_DEV_MODE=false`; the app also forces dev login off
when OIDC is fully configured.

OIDC authenticates identity only. MongoDB remains the authorization source:
staff must already exist in `users`, while invited experts must exist in `people`
and have active `study_access`. Accounts that match neither record are rejected.

Before production, verify staff login, invited expert access, unauthorized-account
rejection, logout, and the inactivity timeout in a non-production Render service.