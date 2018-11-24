









sel <- dt[,.(BUY_RES_AUDUSD,buy_loss_AUDUSD,buy_profit_AUDUSD,sell_loss_AUDUSD,sell_profit_AUDUSD,SELL_RES_AUDUSD,AUDUSD_Close,AUDUSD_High,AUDUSD_Low)]
sel[,':='(AUDUSD_Close=1e4*AUDUSD_Close-7e3,AUDUSD_High=1e4*AUDUSD_High-7e3,AUDUSD_Low=1e4*AUDUSD_Low-7e3)]
sel[,':='(BUY_prof=AUDUSD_Close+18,BUY_loss=AUDUSD_Close-12,SELL_prof=AUDUSD_Close-18,SELL_loss=AUDUSD_Close+12)]
sel[,':='(buy_loss_AUDUSD=buy_loss_AUDUSD/60,buy_profit_AUDUSD=buy_profit_AUDUSD/60,sell_loss_AUDUSD=sell_loss_AUDUSD/60,sell_profit_AUDUSD=sell_profit_AUDUSD/60)]

View(sel)


