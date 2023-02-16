package com.github.apuex.cmcc.cint4;

import java.nio.ByteBuffer;

/**
 * 请求实时数据方式设置
 */
public class SetDynAccessMode extends Message {
    public SetDynAccessMode
    ( int TerminalID
    , int GroupID
    , EnumAccessMode Mode
    , int PollingTime
    , TIDArray Ids
    ) {
        super(EnumPKType.SET_DYN_ACCESS_MODE);
        this.TerminalID = TerminalID;
        this.GroupID = GroupID;
        this.Mode = Mode;
        this.PollingTime = PollingTime;
        this.Ids = Ids;
    }

    public SetDynAccessMode
    ( int SerialNo
    , int TerminalID
    , int GroupID
    , EnumAccessMode Mode
    , int PollingTime
    , TIDArray Ids
    ) {
        super(SerialNo, EnumPKType.SET_DYN_ACCESS_MODE);
        this.TerminalID = TerminalID;
        this.GroupID = GroupID;
        this.Mode = Mode;
        this.PollingTime = PollingTime;
        this.Ids = Ids;
    }

    public int TerminalID; // 上级SCID
    public int GroupID; // 相应模式数据包的序号
    public EnumAccessMode Mode; // 下级实时数据访问的方式
    public int PollingTime; // 定时方式时的发送间隔秒数，小于1无效，若出现小于1的值，则按等于1处理。
    public TIDArray Ids; // 如果类型是站点，即获取站内所有设备下的监控点数据；如果是设备，即获取该设备下所有监控点数据；如果是监控点，即是该点数据。
}

