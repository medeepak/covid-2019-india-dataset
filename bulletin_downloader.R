#Odisha

for(i in seq(12,16)){date=format(Sys.Date()-i,"%d%m%Y"); file_name=paste0("https://health.odisha.gov.in/pdf/news-covid-19-press-meet-",date,"Eng.pdf");download.file(file_name, destfile = file.path("~/Documents/datasciencecourse/github_root/covid19india-officialbulletins/odisha/", basename(file_name)))}