use lec2_gcompu
rename nicon ni 
*Append north america compustat

append using lec2_nacompu
duplicates drop
gen year=year(datadate)
gen month=month(datadate)


sort gvkey year
by gvkey year:  gen obs=_n

drop if obs>1
sort loc gvkey

*Find SIC codes on internet
*https://en.wikipedia.org/wiki/Standard_Industrial_Classification
destring sic,replace
gen sic1=.
replace sic1=1 if sic>=100 &sic<=999
replace sic1=2 if sic>=1000 &sic<=1499
replace sic1=3 if sic>=1500 &sic<=1799
replace sic1=4 if sic>=1800 &sic<=1999
replace sic1=5 if sic>=2000 &sic<=3999
replace sic1=6 if sic>=4000&sic<=4999
replace sic1=7 if sic>=5000&sic<=5199
replace sic1=8 if sic>=5200&sic<=5999
replace sic1=9 if sic>=6000&sic<=6799
replace sic1=10 if sic>=7000&sic<=8999
replace sic1=11 if sic>=9100&sic<=9729
replace sic1=12 if sic>=9900&sic<=9999

*Nouveau
drop if sic<7300 
drop if sic>=7400
drop if missing(at,ni,sale)
drop if at<=0
drop if sale<=0
tab year



snapshot erase _all

joinby sic using sic4name, unm(m)
tab _merge
drop _merge

replace aco=act-che-rect-invt
gen otherassets= at-intan-ppent-act 
replace lco=lct-dlc-ap-txp

order gvkey conm sic year month che rect invt aco act ppent intan otherassets at dlc ap txp lco lct dltt lo txdb itcb mibt pstk ceq mibn mib lse sale cogs xsga xrd xstfws oibdp dp xint nopi spi txt mii dvp ni dvc dvt

drop if missing(at,che,sale,act, ppent,dlc,dltt,ceq,cogs,xsga,oibdp,txt) 

foreach var in at che sale act  ppent dlc dltt ceq cogs xsga txt{
drop if `var'<=0
}


joinby year month curcd using exchange_yahoo2.dta, unm(m)
tab _merge
replace exch_rate=1 if curcd=="USD"
drop if _merge==1 &exch_rate==.
drop _merge
*Convert variables to a common currency
foreach var of varlist che-dvt{ 
*replace `var'=0 if `var'==. 
replace `var'=`var'/exch_rate
}
*Often times when a company does not have R&D expenditure, the database reports missing. So we will set missing R&D back to zero
replace xrd=0 if xrd==.

local j=1
foreach var in che rect invt aco act ppent intan otherassets{
local varlist "che rect invt aco act ppent intan otherassets "
local name:word `j' of `varlist' 
display("processing `name'")
gen `name'_at_ratio=`var'/at
winsor `name'_at_ratio if `name'_at_ratio!=., gen(temp) p(0.02)
drop `name'_at_ratio
rename temp `name'_at_ratio
local j=`j'+1
} 
tabstat che_at_ratio- otherassets_at_ratio ,by(sic1) stat(mean)

local j=1
foreach var in dlc ap txp lco lct dltt lo txdb mibt pstk ceq  {
local varlist "dlc ap txp lco lct dltt lo txdb  mibt pstk ceq "
local name:word `j' of `varlist' 
display("processing `name'")
gen `name'_at_ratio=`var'/at
winsor `name'_at_ratio if `name'_at_ratio!=., gen(temp) p(0.02)
drop `name'_at_ratio
rename temp `name'_at_ratio
local j=`j'+1
} 

tabstat dlc_at_ratio-ceq_at_ratio ,by(sic1) stat(mean)
sum ceq_at_ratio if sic1==12,d

local j=1
foreach var in cogs xsga xrd xstfws oibdp dp xint nopi spi txt mii dvp ni {
local varlist "cogs xsga xrd xstfws oibdp dp xint nopi spi txt mii dvp ni "
local name:word `j' of `varlist' 
display("processing `name'")
gen `name'_sale_ratio=`var'/sale
winsor `name'_sale_ratio if `name'_sale_ratio!=., gen(temp) p(0.02)
drop `name'_sale_ratio
rename temp `name'_sale_ratio
local j=`j'+1
} 

tabstat cogs_sale_ratio-ni_sale_ratio ,by(sic1) stat(mean)

tabstat cogs_sale_ratio-ni_sale_ratio  ,by(sic1) stat(median)


*Create Financial Ratios
encode gvkey, gen(firm)
xtset firm year
by firm: gen growth_sale=sale/sale[_n-1]-1
gen gross_margin= (sale-cogs-xsga)/sale
gen net_margin= ni/sale
gen roa=oibdp/at
gen roe=ni/(pstk+ ceq +mibt)
*asset turnover
gen at_turnover=sale/at 
*Accounts receivable sales
gen ard_ratio=rect/(sale/360) 

*test
gen incor_ratio = (dp+xint+txt+ni)/xint
gen quick_ratio = (act-invt)/lct

*Book leverage ratio
gen blev_ratio= (dlc+dltt)/at 


 *now winsorize them
foreach var in growth_sale gross_margin net_margin roa roe at_turnover ard_ratio incor_ratio quick_ratio blev_ratio {
winsor `var', gen(temp) p(0.02)
replace `var'=temp
drop temp
} 

gen log_at=log(at)
keep firm year growth_sale-blev_ratio log_at  intan_at_ratio sic conm
gen firm2=firm
drop firm
rename firm2 firm
sort year firm

order year firm sic growth_sale-blev_ratio log_at conm

foreach var of varlist year-log_at{
drop if `var'==.
}
*Facebook is company 535

*log
xtset firm year
by firm: gen Lroa=L.roa
by firm:gen Lgrowth_sale=L.growth_sale
by firm:gen L2growth_sale=L2.growth_sale
by firm:gen Lgross_margin=L.gross_margin
by firm: gen Lintan_at=L.intan_at

*Generate sector specific info
*Generate sic2 codes
tostring sic, gen (sicstr)
gen sic2=substr(sicstr,1,2)
destring sic2,replace

*Sort by year ,sector
sort year sic2 
*Find the average growth of sales within sector for every year
by year sic2: egen mean_growth_sale=mean(growth_sale)
*Regression on single firm data- AR(1)
xtset year firm
reg growth_sale Lgrowth_sale if firm==535

*Regression on all firm data- no fixed effects 
reg growth_sale Lgrowth_sale 
*Regression on all firm data AR(2)- no fixed effects
reg growth_sale Lgrowth_sale L2growth_sale 

*Regression on all firm data AR(1), panel data
xtset firm year
xtdpdsys growth_sale, lags(1) 
xi:xtdpdsys growth_sale i.year, lags(1) 
xi:xtdpdsys growth_sale  i.year ,lags(1) pre(Lroa Lgross_margin) 
xi:xtdpdsys roa, lags(1)
xi:xtdpdsys gross_margin, lags(1)
*Multilevel AR(1)
gen dev_growth=growth_sale-mean_growth_sale
by firm : gen Ldev_growth=L.dev_growth
reg dev_growth L.dev_growth
*Eliminate constant
reg dev_growth L.dev_growth, nocons
save data503,replace
*Keep the sector averages for sale growth per year
collapse  (mean) growth_sale, by(year sic2)
sort sic2 year
rename growth_sale mean_growth_sale
*Perform a panel dynamic panel regression, on sector averages
xtset sic2 year
xtdpdsys mean_growth_sale, lags(1)

*Restricted Panel VAR
use data503,clear
xi:xtdpdsys growth_sale, lags(1) pre(Lroa Lgross_margin)
xi:xtdpdsys roa, lags(1) 
xi:xtdpdsys gross_margin, lags(1) 

xi:xtdpdsys growth_sale, lags(1) pre(Lintan_at Lgross_margin)
xi:xtdpdsys intan_at, lags(1) 
xi:xtdpdsys gross_margin, lags(1) 
