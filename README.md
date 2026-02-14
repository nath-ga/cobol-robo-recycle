# RoboRecycle COBOL BOM Cost Calculation

## Overview

This project simulates a small manufacturing system for recycling robots.

It reads:

- `parts.csv` component master data (part number, name, unit cost)
- `robots.csv` robot master data (robot number, name, production quantity)
- `bom.csv` bill of materials (robot number, part number, quantity per robot)

The program calculates:

- Material cost per robot
- Total material cost based on production quantity

The implementation is written in COBOL using GnuCOBOL and follows a structured, procedural design.

## Technical Features

- CSV file processing (line sequential)
- Table structures using OCCURS
- Lookup logic for master data resolution
- Bill of Materials cost calculation
- Separation of logical processing blocks (LOAD, PARSE, CALCULATE)

## Structure

cobol-robo-recycle/
│
├── data/
│ ├── parts.csv
│ ├── robots.csv
│ └── bom.csv
│
├── src/
│ └── calc_costs.cbl
│
└── README.md

## How to Compile and Run

Using GnuCOBOL:

```bash
cobc -x calc_costs.cbl
./calc_costs

## Purpose

This project demonstrates:

Reactivation of COBOL knowledge
Handling of structured business data
Implementation of cost calculation logic
Working with classic fixed-format COBOL