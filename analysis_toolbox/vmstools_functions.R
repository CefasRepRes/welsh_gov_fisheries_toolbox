# Functions extracted from library(vmstools) . 
# These functions are relevant to the Welsh Gov Fisheries Toolkit

 
splitAmongPings =
    function (tacsat, eflalo, variable = "all", level = "day", conserve = TRUE, 
              by = NULL, returnAll = T) 
    {
      require(data.table)
      if (!"FT_REF" %in% colnames(tacsat)) 
        stop("tacsat file needs FT_REF detailing trip number")
      if (!"SI_STATE" %in% colnames(tacsat)) 
        stop("tacsat file needs SI_STATE detailing activity of vessel")
      if (level == "trip" & conserve == TRUE) 
        stop("conserve catches only at level = ICESrectangle or day")
      if (!"SI_DATIM" %in% colnames(tacsat)) 
        tacsat$SI_DATIM <- as.POSIXct(paste(tacsat$SI_DATE, tacsat$SI_TIME, 
                                            sep = " "), tz = "GMT", format = "%d/%m/%Y  %H:%M")
      if (!"LE_CDATIM" %in% colnames(eflalo)) 
        eflalo$LE_CDATIM <- as.POSIXct(eflalo$LE_CDAT, tz = "GMT", 
                                       format = "%d/%m/%Y")
      if (is.null(by) == FALSE) {
        if (any(is.na(tacsat[, by])) | any(tacsat[, by] == 0)) 
          stop("'by' column in tacsat contains NA or zero's. Cannot execute with NA's or zeros")
      }
      if (level == "day") {
        level <- c("day", "ICESrectangle", "trip")
      }
      else {
        if (level == "ICESrectangle") {
          level <- c("ICESrectangle", "trip")
        }
        else {
          if (level == "trip") {
            level <- c("trip")
          }
        }
      }
      tacsat$ID <- 1:nrow(tacsat)
      eflaloCol <- colnames(eflalo)
      kgs <- grep("LE_KG", colnames(eflalo))
      eur <- grep("LE_EURO", colnames(eflalo))
      remtacsat <- subset(tacsat, SI_STATE == 0)
      tacsat <- subset(tacsat, SI_STATE != 0)
      tacsatTrip <- subset(tacsat, FT_REF != 0)
      remainTacsat <- sort(unique(tacsatTrip$ID))
      eflalo$ID <- 1:nrow(eflalo)
      eflaloTrip <- subset(eflalo, FT_REF %in% sort(unique(tacsatTrip$FT_REF)) & 
                             VE_REF %in% sort(unique(tacsatTrip$VE_REF)))
      eflaloNoTrip <- eflalo[which(!eflalo$ID %in% eflaloTrip$ID), 
                             -match("ID", colnames(eflalo))]
      eflaloVessel <- eflaloNoTrip[which(paste(eflaloNoTrip$VE_REF, 
                                               format(eflaloNoTrip$LE_CDATIM, "%Y")) %in% unique(paste(tacsatTrip$VE_REF, 
                                                                                                       format(tacsatTrip$SI_DATIM, "%Y")))), ]
      eflaloNoVessel <- eflaloNoTrip[which(!paste(eflaloNoTrip$VE_REF, 
                                                  format(eflaloNoTrip$LE_CDATIM, "%Y")) %in% unique(paste(tacsatTrip$VE_REF, 
                                                                                                          format(tacsatTrip$SI_DATIM, "%Y")))), ]
      if (dim(tacsatTrip)[1] > 0 & dim(eflaloTrip)[1] > 0) {
        if ("day" %in% level) {
          print("level: day")
          if (!"SI_YEAR" %in% colnames(tacsatTrip)) 
            tacsatTrip$SI_YEAR <- an(format(tacsatTrip$SI_DATIM, 
                                            format = "%Y"))
          if (!"SI_DAY" %in% colnames(tacsatTrip)) 
            tacsatTrip$SI_DAY <- an(format(tacsatTrip$SI_DATIM, 
                                           format = "%j"))
          if (!"LE_RECT" %in% colnames(tacsatTrip)) 
            tacsatTrip$LE_RECT <- ICESrectangle(tacsatTrip)
          if (!"SI_YEAR" %in% colnames(eflaloTrip)) 
            eflaloTrip$SI_YEAR <- an(format(eflaloTrip$LE_CDATIM, 
                                            format = "%Y"))
          if (!"SI_DAY" %in% colnames(eflaloTrip)) 
            eflaloTrip$SI_DAY <- an(format(eflaloTrip$LE_CDATIM, 
                                           format = "%j"))
          nPings <- countPings(~year + VE_REF + FT_REF + icesrectangle + 
                                 day, tacsatTrip, by = by)
          res <- eflalo2Pings(eflaloTrip, tacsatTrip, nPings, 
                              c("SI_YEAR", "VE_REF", "FT_REF", "LE_RECT", "SI_DAY"), 
                              eflaloCol[c(kgs, eur)], remainTacsat, by = by)
          eflaloTrip <- res[["eflalo"]]
          byDayTacsat <- res[["tacsat"]]
          remainTacsat <- res[["remainTacsat"]]
        }
        if ("ICESrectangle" %in% level) {
          print("level: rectangle")
          if (!"SI_YEAR" %in% colnames(tacsatTrip)) 
            tacsatTrip$SI_YEAR <- an(format(tacsatTrip$SI_DATIM, 
                                            format = "%Y"))
          if (!"LE_RECT" %in% colnames(tacsatTrip)) 
            tacsatTrip$LE_RECT <- ICESrectangle(tacsatTrip)
          if (!"SI_YEAR" %in% colnames(eflaloTrip)) 
            eflaloTrip$SI_YEAR <- an(format(eflaloTrip$LE_CDATIM, 
                                            format = "%Y"))
          nPings <- countPings(~year + VE_REF + FT_REF + icesrectangle, 
                               tacsatTrip, by = by)
          res <- eflalo2Pings(eflaloTrip, tacsatTrip, nPings, 
                              c("SI_YEAR", "VE_REF", "FT_REF", "LE_RECT"), 
                              eflaloCol[c(kgs, eur)], remainTacsat, by = by)
          eflaloTrip <- res[["eflalo"]]
          byRectTacsat <- res[["tacsat"]]
          remainTacsat <- res[["remainTacsat"]]
        }
        if ("trip" %in% level) {
          print("level: trip")
          if (!"SI_YEAR" %in% colnames(tacsatTrip)) 
            tacsatTrip$SI_YEAR <- an(format(tacsatTrip$SI_DATIM, 
                                            format = "%Y"))
          if (!"SI_YEAR" %in% colnames(eflaloTrip)) 
            eflaloTrip$SI_YEAR <- an(format(eflaloTrip$LE_CDATIM, 
                                            format = "%Y"))
          nPings <- countPings(~year + VE_REF + FT_REF, tacsatTrip, 
                               by = by)
          res <- eflalo2Pings(eflaloTrip, tacsatTrip, nPings, 
                              c("SI_YEAR", "VE_REF", "FT_REF"), eflaloCol[c(kgs, 
                                                                            eur)], remainTacsat, by = by)
          eflaloTrip <- res[["eflalo"]]
          byTripTacsat <- res[["tacsat"]]
          remainTacsat <- res[["remainTacsat"]]
        }
        if (length(remainTacsat) > 0) 
          warning("Not all tacsat records with tripnumber have been merged!!")
        if (nrow(eflaloTrip) > 0) 
          warning("Not all eflalo records with matching VMS tripnumber have been merged!!")
        if ("day" %in% level) {
          tacsatFTREF <- rbind(byDayTacsat, byRectTacsat, byTripTacsat)
        }
        else {
          if ("ICESrectangle" %in% level) {
            tacsatFTREF <- rbind(byRectTacsat, byTripTacsat)
          }
          else {
            tacsatFTREF <- byTripTacsat
          }
        }
        tacsatFTREF[, kgeur(colnames(tacsatFTREF))] <- sweep(tacsatFTREF[, 
                                                                         kgeur(colnames(tacsatFTREF))], 1, tacsatFTREF$pings, 
                                                             "/")
        tacsatFTREF$ID <- af(ac(tacsatFTREF$ID.x))
        DT <- data.table(tacsatFTREF)
        eq1 <- c.listquote(paste("sum(", colnames(tacsatFTREF[, 
                                                              kgeur(colnames(tacsatFTREF))]), ",na.rm=TRUE)", sep = ""))
        tacsatFTREF <- DT[, eval(eq1), by = ID.x]
        tacsatFTREF <- data.frame(tacsatFTREF)
        setnames(tacsatFTREF, colnames(tacsatFTREF), c("ID", 
                                                       colnames(eflaloTrip[, kgeur(colnames(eflaloTrip))])))
      }
      if (conserve == TRUE) {
        if (dim(tacsat)[1] > 0 & dim(eflaloVessel)[1] > 0) {
          if ("day" %in% level) {
            print("level: day & conserve = T, by vessel")
            if (!"SI_YEAR" %in% colnames(tacsat)) 
              tacsat$SI_YEAR <- an(format(tacsat$SI_DATIM, 
                                          format = "%Y"))
            if (!"SI_DAY" %in% colnames(tacsat)) 
              tacsat$SI_DAY <- an(format(tacsat$SI_DATIM, 
                                         format = "%j"))
            if (!"LE_RECT" %in% colnames(tacsat)) 
              tacsat$LE_RECT <- ICESrectangle(tacsat)
            if (!"SI_YEAR" %in% colnames(eflaloVessel)) 
              eflaloVessel$SI_YEAR <- an(format(eflaloVessel$LE_CDATIM, 
                                                format = "%Y"))
            if (!"SI_DAY" %in% colnames(eflaloVessel)) 
              eflaloVessel$SI_DAY <- an(format(eflaloVessel$LE_CDATIM, 
                                               format = "%j"))
            nPings <- countPings(~year + VE_REF + icesrectangle + 
                                   day, tacsat, by = by)
            res <- eflalo2Pings(eflaloVessel, tacsat, nPings, 
                                c("SI_YEAR", "VE_REF", "LE_RECT", "SI_DAY"), 
                                eflaloCol[c(kgs, eur)], NULL, by = by)
            eflaloVessel <- res[["eflalo"]]
            byDayTacsat <- res[["tacsat"]]
          }
          if ("ICESrectangle" %in% level) {
            print("level: rectangle & conserve = T, by vessel")
            if (!"SI_YEAR" %in% colnames(tacsat)) 
              tacsat$SI_YEAR <- an(format(tacsat$SI_DATIM, 
                                          format = "%Y"))
            if (!"LE_RECT" %in% colnames(tacsat)) 
              tacsat$LE_RECT <- ICESrectangle(tacsat)
            if (!"SI_YEAR" %in% colnames(eflaloVessel)) 
              eflaloVessel$SI_YEAR <- an(format(eflaloVessel$LE_CDATIM, 
                                                format = "%Y"))
            nPings <- countPings(~year + VE_REF + icesrectangle, 
                                 tacsat, by = by)
            res <- eflalo2Pings(eflaloVessel, tacsat, nPings, 
                                c("SI_YEAR", "VE_REF", "LE_RECT"), eflaloCol[c(kgs, 
                                                                               eur)], NULL, by = by)
            eflaloVessel <- res[["eflalo"]]
            byRectTacsat <- res[["tacsat"]]
          }
          if (TRUE) {
            print("level: year & conserve = T, by vessel")
            if (!"SI_YEAR" %in% colnames(tacsat)) 
              tacsat$SI_YEAR <- an(format(tacsat$SI_DATIM, 
                                          format = "%Y"))
            if (!"SI_YEAR" %in% colnames(eflaloVessel)) 
              eflaloVessel$SI_YEAR <- an(format(eflaloVessel$LE_CDATIM, 
                                                format = "%Y"))
            nPings <- countPings(~year + VE_REF, tacsat, 
                                 by = by)
            res <- eflalo2Pings(eflaloVessel, tacsat, nPings, 
                                c("SI_YEAR", "VE_REF"), eflaloCol[c(kgs, eur)], 
                                NULL, by = by)
            eflaloVessel <- res[["eflalo"]]
            byVessTacsat <- res[["tacsat"]]
          }
          if ("day" %in% level) {
            tacsatVEREF <- rbind(byDayTacsat, byRectTacsat, 
                                 byVessTacsat)
          }
          else {
            if ("ICESrectangle" %in% level) {
              tacsatVEREF <- rbind(byRectTacsat, byVessTacsat)
            }
            else {
              tacsatVEREF <- byVessTacsat
            }
          }
          tacsatVEREF[, kgeur(colnames(tacsatVEREF))] <- sweep(tacsatVEREF[, 
                                                                           kgeur(colnames(tacsatVEREF))], 1, tacsatVEREF$pings, 
                                                               "/")
          tacsatVEREF$ID <- af(ac(tacsatVEREF$ID.x))
          DT <- data.table(tacsatVEREF)
          eq1 <- c.listquote(paste("sum(", colnames(tacsatVEREF[, 
                                                                kgeur(colnames(tacsatVEREF))]), ",na.rm=TRUE)", 
                                   sep = ""))
          tacsatVEREF <- DT[, eval(eq1), by = ID.x]
          tacsatVEREF <- data.frame(tacsatVEREF)
          setnames(tacsatVEREF, colnames(tacsatVEREF), c("ID", 
                                                         colnames(eflaloVessel[, kgeur(colnames(eflaloVessel))])))
        }
        if (dim(tacsat)[1] > 0 & dim(eflaloNoVessel)[1] > 0) {
          if ("day" %in% level) {
            print("level: day & conserve = T, no vessel match")
            if (!"SI_YEAR" %in% colnames(tacsat)) 
              tacsat$SI_YEAR <- an(format(tacsat$SI_DATIM, 
                                          format = "%Y"))
            if (!"SI_DAY" %in% colnames(tacsat)) 
              tacsat$SI_DAY <- an(format(tacsat$SI_DATIM, 
                                         format = "%j"))
            if (!"LE_RECT" %in% colnames(tacsat)) 
              tacsat$LE_RECT <- ICESrectangle(tacsat)
            if (!"SI_YEAR" %in% colnames(eflaloNoVessel)) 
              eflaloNoVessel$SI_YEAR <- an(format(eflaloNoVessel$LE_CDATIM, 
                                                  format = "%Y"))
            if (!"SI_DAY" %in% colnames(eflaloNoVessel)) 
              eflaloNoVessel$SI_DAY <- an(format(eflaloNoVessel$LE_CDATIM, 
                                                 format = "%j"))
            nPings <- countPings(~year + icesrectangle + 
                                   day, tacsat, by = by)
            res <- eflalo2Pings(eflaloNoVessel, tacsat, nPings, 
                                c("SI_YEAR", "LE_RECT", "SI_DAY"), eflaloCol[c(kgs, 
                                                                               eur)], NULL, by = by)
            eflaloNoVessel <- res[["eflalo"]]
            byDayTacsat <- res[["tacsat"]]
          }
          if ("ICESrectangle" %in% level) {
            print("level: rectangle & conserve = T, no vessel match")
            if (!"SI_YEAR" %in% colnames(tacsat)) 
              tacsat$SI_YEAR <- an(format(tacsat$SI_DATIM, 
                                          format = "%Y"))
            if (!"LE_RECT" %in% colnames(tacsat)) 
              tacsat$LE_RECT <- ICESrectangle(tacsat)
            if (!"SI_YEAR" %in% colnames(eflaloNoVessel)) 
              eflaloNoVessel$SI_YEAR <- an(format(eflaloNoVessel$LE_CDATIM, 
                                                  format = "%Y"))
            nPings <- countPings(~year + icesrectangle, tacsat, 
                                 by = by)
            res <- eflalo2Pings(eflaloNoVessel, tacsat, nPings, 
                                c("SI_YEAR", "LE_RECT"), eflaloCol[c(kgs, eur)], 
                                NULL, by = by)
            eflaloNoVessel <- res[["eflalo"]]
            byRectTacsat <- res[["tacsat"]]
          }
          if (TRUE) {
            print("level: year & conserve = T, no vessel match")
            if (!"SI_YEAR" %in% colnames(tacsat)) 
              tacsat$SI_YEAR <- an(format(tacsat$SI_DATIM, 
                                          format = "%Y"))
            if (!"SI_YEAR" %in% colnames(eflaloNoVessel)) 
              eflaloNoVessel$SI_YEAR <- an(format(eflaloNoVessel$LE_CDATIM, 
                                                  format = "%Y"))
            nPings <- countPings(~year, tacsat, by = by)
            res <- eflalo2Pings(eflaloNoVessel, tacsat, nPings, 
                                c("SI_YEAR"), eflaloCol[c(kgs, eur)], NULL, 
                                by = by)
            eflaloNoVessel <- res[["eflalo"]]
            byVessTacsat <- res[["tacsat"]]
          }
          if ("day" %in% level) {
            tacsatREF <- rbind(byDayTacsat, byRectTacsat, 
                               byVessTacsat)
          }
          else {
            if ("ICESrectangle" %in% level) {
              tacsatREF <- rbind(byRectTacsat, byVessTacsat)
            }
            else {
              tacsatREF <- byVessTacsat
            }
          }
          tacsatREF[, kgeur(colnames(tacsatREF))] <- sweep(tacsatREF[, 
                                                                     kgeur(colnames(tacsatREF))], 1, tacsatREF$pings, 
                                                           "/")
          tacsatREF$ID <- af(ac(tacsatREF$ID.x))
          DT <- data.table(tacsatREF)
          eq1 <- c.listquote(paste("sum(", colnames(tacsatREF[, 
                                                              kgeur(colnames(tacsatREF))]), ",na.rm=TRUE)", 
                                   sep = ""))
          tacsatREF <- DT[, eval(eq1), by = ID.x]
          tacsatREF <- data.frame(tacsatREF)
          setnames(tacsatREF, colnames(tacsatREF), c("ID", 
                                                     colnames(eflaloVessel[, kgeur(colnames(eflaloVessel))])))
        }
      }
      if (conserve == TRUE) {
        if (exists("tacsatFTREF")) {
          one <- tacsatFTREF
        }
        else {
          one <- numeric()
        }
        if (exists("tacsatVEREF")) {
          two <- tacsatVEREF
        }
        else {
          two <- numeric()
        }
        if (exists("tacsatREF")) {
          three <- tacsatREF
        }
        else {
          three <- numeric()
        }
        tacsatTot <- rbind(one, two, three)
        DT <- data.table(tacsatTot)
        eq1 <- c.listquote(paste("sum(", colnames(tacsatTot[, 
                                                            kgeur(colnames(tacsatTot))]), ",na.rm=TRUE)", sep = ""))
        tacsatTot <- DT[, eval(eq1), by = ID]
        tacsatTot <- data.frame(tacsatTot)
        setnames(tacsatTot, colnames(tacsatTot), c("ID", colnames(eflalo[, 
                                                                         kgeur(colnames(eflalo))])))
        tacsatReturn <- merge(tacsat, tacsatTot, by = "ID", all.x = TRUE)
        if (variable == "value") 
          tacsatReturn <- tacsatReturn[, c(1:dim(tacsat)[2], 
                                           grep("EURO", colnames(tacsatReturn)))]
        if (variable == "kgs") 
          tacsatReturn <- tacsatReturn[, c(1:dim(tacsat)[2], 
                                           grep("KG", colnames(tacsatReturn)))]
        if (variable == "all") 
          tacsatReturn <- tacsatReturn
      }
      else {
        if (exists("tacsatFTREF") == FALSE) {
          stop("You have selected not to conserve catches, but there is no trip identifier in the tacsat file")
        }
        tacsatReturn <- merge(tacsat, tacsatFTREF, by = "ID", 
                              all.x = TRUE)
        if (variable == "value") 
          tacsatReturn <- tacsatReturn[, c(1:dim(tacsat)[2], 
                                           grep("EURO", colnames(tacsatReturn)))]
        if (variable == "kgs") 
          tacsatReturn <- tacsatReturn[, c(1:dim(tacsat)[2], 
                                           grep("KG", colnames(tacsatReturn)))]
        if (variable == "all") 
          tacsatReturn <- tacsatReturn
      }
      if (returnAll & nrow(remtacsat) > 0) 
        tacsatReturn <- orderBy(~ID, data = rbindTacsat(tacsatReturn, 
                                                        remtacsat))
      return(orderBy(~ID, data = tacsatReturn)[, -match("ID", colnames(tacsatReturn))])
    }




CSquare = function (lon, lat, degrees) 
            {
              if (length(lon) != length(lat)) 
                stop("length of longitude not equal to length of latitude")
              if (!degrees %in% c(10, 5, 1, 0.5, 0.1, 0.05, 0.01)) 
                stop("degrees specified not in range: c(10,5,1,0.5,0.1,0.05,0.01)")
              dims <- length(lon)
              quadrants <- array(NA, dim = c(4, 6, dims), dimnames = list(c("globalQuadrant", 
                                                                            "intmQuadrant1", "intmQuadrant2", "intmQuadrant3"), c("quadrantDigit", 
                                                                                                                                  "latDigit", "lonDigit", "latRemain", "lonRemain", "code"), 
                                                                          seq(1, dims, 1)))
              quadrants["globalQuadrant", "quadrantDigit", ] <- 4 - (((2 * 
                                                                         floor(1 + (lon/200))) - 1) * ((2 * floor(1 + (lat/200))) + 
                                                                                                         1))
              quadrants["globalQuadrant", "latDigit", ] <- floor(abs(lat)/10)
              quadrants["globalQuadrant", "lonDigit", ] <- floor(abs(lon)/10)
              quadrants["globalQuadrant", "latRemain", ] <- round(abs(lat) - 
                                                                    (quadrants["globalQuadrant", "latDigit", ] * 10), 7)
              quadrants["globalQuadrant", "lonRemain", ] <- round(abs(lon) - 
                                                                    (quadrants["globalQuadrant", "lonDigit", ] * 10), 7)
              quadrants["globalQuadrant", "code", ] <- quadrants["globalQuadrant", 
                                                                 "quadrantDigit", ] * 1000 + quadrants["globalQuadrant", 
                                                                                                       "latDigit", ] * 100 + quadrants["globalQuadrant", "lonDigit", 
                                                                                                       ]
              quadrants["intmQuadrant1", "quadrantDigit", ] <- (2 * floor(quadrants["globalQuadrant", 
                                                                                    "latRemain", ] * 0.2)) + floor(quadrants["globalQuadrant", 
                                                                                                                             "lonRemain", ] * 0.2) + 1
              quadrants["intmQuadrant1", "latDigit", ] <- floor(quadrants["globalQuadrant", 
                                                                          "latRemain", ])
              quadrants["intmQuadrant1", "lonDigit", ] <- floor(quadrants["globalQuadrant", 
                                                                          "lonRemain", ])
              quadrants["intmQuadrant1", "latRemain", ] <- round((quadrants["globalQuadrant", 
                                                                            "latRemain", ] - quadrants["intmQuadrant1", "latDigit", 
                                                                            ]) * 10, 7)
              quadrants["intmQuadrant1", "lonRemain", ] <- round((quadrants["globalQuadrant", 
                                                                            "lonRemain", ] - quadrants["intmQuadrant1", "lonDigit", 
                                                                            ]) * 10, 7)
              quadrants["intmQuadrant1", "code", ] <- quadrants["intmQuadrant1", 
                                                                "quadrantDigit", ] * 100 + quadrants["intmQuadrant1", 
                                                                                                     "latDigit", ] * 10 + quadrants["intmQuadrant1", "lonDigit", 
                                                                                                     ]
              quadrants["intmQuadrant2", "quadrantDigit", ] <- (2 * floor(quadrants["intmQuadrant1", 
                                                                                    "latRemain", ] * 0.2)) + floor(quadrants["intmQuadrant1", 
                                                                                                                             "lonRemain", ] * 0.2) + 1
              quadrants["intmQuadrant2", "latDigit", ] <- floor(quadrants["intmQuadrant1", 
                                                                          "latRemain", ])
              quadrants["intmQuadrant2", "lonDigit", ] <- floor(quadrants["intmQuadrant1", 
                                                                          "lonRemain", ])
              quadrants["intmQuadrant2", "latRemain", ] <- round((quadrants["intmQuadrant1", 
                                                                            "latRemain", ] - quadrants["intmQuadrant2", "latDigit", 
                                                                            ]) * 10, 7)
              quadrants["intmQuadrant2", "lonRemain", ] <- round((quadrants["intmQuadrant1", 
                                                                            "lonRemain", ] - quadrants["intmQuadrant2", "lonDigit", 
                                                                            ]) * 10, 7)
              quadrants["intmQuadrant2", "code", ] <- quadrants["intmQuadrant2", 
                                                                "quadrantDigit", ] * 100 + quadrants["intmQuadrant2", 
                                                                                                     "latDigit", ] * 10 + quadrants["intmQuadrant2", "lonDigit", 
                                                                                                     ]
              quadrants["intmQuadrant3", "quadrantDigit", ] <- (2 * floor(quadrants["intmQuadrant2", 
                                                                                    "latRemain", ] * 0.2)) + floor(quadrants["intmQuadrant2", 
                                                                                                                             "lonRemain", ] * 0.2) + 1
              quadrants["intmQuadrant3", "latDigit", ] <- floor(quadrants["intmQuadrant2", 
                                                                          "latRemain", ])
              quadrants["intmQuadrant3", "lonDigit", ] <- floor(quadrants["intmQuadrant2", 
                                                                          "lonRemain", ])
              quadrants["intmQuadrant3", "latRemain", ] <- round((quadrants["intmQuadrant2", 
                                                                            "latRemain", ] - quadrants["intmQuadrant3", "latDigit", 
                                                                            ]) * 10, 7)
              quadrants["intmQuadrant3", "lonRemain", ] <- round((quadrants["intmQuadrant2", 
                                                                            "lonRemain", ] - quadrants["intmQuadrant3", "lonDigit", 
                                                                            ]) * 10, 7)
              quadrants["intmQuadrant3", "code", ] <- quadrants["intmQuadrant3", 
                                                                "quadrantDigit", ] * 100 + quadrants["intmQuadrant3", 
                                                                                                     "latDigit", ] * 10 + quadrants["intmQuadrant3", "lonDigit", 
                                                                                                     ]
              if (degrees == 10) 
                CSquareCodes <- quadrants["globalQuadrant", "code", ]
              if (degrees == 5) 
                CSquareCodes <- paste(quadrants["globalQuadrant", "code", 
                ], ":", quadrants["intmQuadrant1", "quadrantDigit", 
                ], sep = "")
              if (degrees == 1) 
                CSquareCodes <- paste(quadrants["globalQuadrant", "code", 
                ], ":", quadrants["intmQuadrant1", "code", ], sep = "")
              if (degrees == 0.5) 
                CSquareCodes <- paste(quadrants["globalQuadrant", "code", 
                ], ":", quadrants["intmQuadrant1", "code", ], ":", 
                quadrants["intmQuadrant2", "quadrantDigit", ], sep = "")
              if (degrees == 0.1) 
                CSquareCodes <- paste(quadrants["globalQuadrant", "code", 
                ], ":", quadrants["intmQuadrant1", "code", ], ":", 
                quadrants["intmQuadrant2", "code", ], sep = "")
              if (degrees == 0.05) 
                CSquareCodes <- paste(quadrants["globalQuadrant", "code", 
                ], ":", quadrants["intmQuadrant1", "code", ], ":", 
                quadrants["intmQuadrant2", "code", ], ":", quadrants["intmQuadrant3", 
                                                                     "quadrantDigit", ], sep = "")
              if (degrees == 0.01) 
                CSquareCodes <- paste(quadrants["globalQuadrant", "code", 
                ], ":", quadrants["intmQuadrant1", "code", ], ":", 
                quadrants["intmQuadrant2", "code", ], ":", quadrants["intmQuadrant3", 
                                                                     "code", ], sep = "")
              return(CSquareCodes)
            }


CSquare2LonLat = function (csqr, degrees) 
                  {
                    ra <- 1e-06
                    chars <- an(nchar(csqr))
                    tensqd <- an(substr(csqr, 1, 1)) + ra
                    gqlat <- (round(abs(tensqd - 4) * 2/10) * 10/5) - 1
                    tenslatd <- an(substr(csqr, 2, 2)) + ra
                    gqlon <- (2 * round(tensqd/10) - 1) * -1
                    tenslond <- an(substr(csqr, 3, 4)) + ra
                    unitsqd <- an(substr(csqr, 6, 6)) + ra
                    iqulat <- round(unitsqd * 2/10)
                    unitslatd <- an(substr(csqr, 7, 7)) + ra
                    iqulon <- (round((unitsqd - 1)/2, 1) - floor((unitsqd - 1)/2)) * 
                      2
                    unitslond <- an(substr(csqr, 8, 8)) + ra
                    tenthsqd <- an(substr(csqr, 10, 10)) + ra
                    iqtlat <- round(tenthsqd * 2/10)
                    tenthslatd <- an(substr(csqr, 11, 11)) + ra
                    iqtlon <- (round((tenthsqd - 1)/2, 1) - floor((tenthsqd - 
                                                                     1)/2)) * 2
                    tenthslond <- an(substr(csqr, 12, 12)) + ra
                    hundqd <- an(substr(csqr, 14, 14)) + ra
                    iqhlat <- round(hundqd * 2/10)
                    hundlatd <- an(substr(csqr, 15, 15)) + ra
                    iqhlon <- (round((hundqd - 1)/2, 1) - floor((hundqd - 1)/2)) * 
                      2
                    hundlond <- an(substr(csqr, 16, 16)) + ra
                    reso <- 10^(1 - floor((chars - 4)/4)) - ((round((chars - 
                                                                       4)/4, 1) - floor((chars - 4)/4)) * 10^(1 - floor((chars - 
                                                                                                                           4)/4)))
                    if (degrees < reso[1]) 
                      stop("Returning degrees is smaller than format of C-square")
                    if (degrees == 10) {
                      lat <- ((tenslatd * 10) + 5) * gqlat - ra
                      lon <- ((tenslond * 10) + 5) * gqlon - ra
                    }
                    if (degrees == 5) {
                      lat <- ((tenslatd * 10) + (iqulat * 5) + 2.5) * gqlat - 
                        ra
                      lon <- ((tenslond * 10) + (iqulon * 5) + 2.5) * gqlon - 
                        ra
                    }
                    if (degrees == 1) {
                      lat <- ((tenslatd * 10) + unitslatd + 0.5) * gqlat - 
                        ra
                      lon <- ((tenslond * 10) + unitslond + 0.5) * gqlon - 
                        ra
                    }
                    if (degrees == 0.5) {
                      lat <- ((tenslatd * 10) + unitslatd + (iqtlat * 0.5) + 
                                0.25) * gqlat - ra
                      lon <- ((tenslond * 10) + unitslond + (iqtlon * 0.5) + 
                                0.25) * gqlon - ra
                    }
                    if (degrees == 0.1) {
                      lat <- ((tenslatd * 10) + unitslatd + (tenthslatd * 0.1) + 
                                0.05) * gqlat - ra
                      lon <- ((tenslond * 10) + unitslond + (tenthslond * 0.1) + 
                                0.05) * gqlon - ra
                    }
                    if (degrees == 0.05) {
                      lat <- ((tenslatd * 10) + unitslatd + (tenthslatd * 0.1) + 
                                (iqhlat * 0.05) + 0.025) * gqlat - ra
                      lon <- ((tenslond * 10) + unitslond + (tenthslond * 0.1) + 
                                (iqhlon * 0.05) + 0.025) * gqlon - ra
                    }
                    if (degrees == 0.01) {
                      lat <- ((tenslatd * 10) + unitslatd + (tenthslatd * 0.1) + 
                                (hundlatd * 0.01) + 0.005) * gqlat - ra
                      lon <- ((tenslond * 10) + unitslond + (tenthslond * 0.1) + 
                                (hundlond * 0.01) + 0.005) * gqlon - ra
                    }
                    return(data.frame(SI_LATI = lat, SI_LONG = lon))
                  }
