package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 请求告警数据方式设置
 */
public class SetAlarmMode extends Message {
    public SetAlarmMode
    ( int GroupID
    , EnumAlarmMode Mode
    , TIDArray Ids
    ) {
        super(EnumPKType.SET_ALARM_MODE);
        this.GroupID = GroupID;
        this.Mode = Mode;
        this.Ids = Ids;
    }

    public SetAlarmMode
    ( int SerialNo
    , int GroupID
    , EnumAlarmMode Mode
    , TIDArray Ids
    ) {
        super(SerialNo, EnumPKType.SET_ALARM_MODE);
        this.GroupID = GroupID;
        this.Mode = Mode;
        this.Ids = Ids;
    }

    public int GroupID; // 相应模式数据包的序号
    public EnumAlarmMode Mode; // 告警等级设定的模式
    public TIDArray Ids; // 如果类型是站点，即获取站内所有设备下的监控点数据；如果是设备，即获取该设备下所有监控点数据；如果是监控点，即是该点数据。
}

