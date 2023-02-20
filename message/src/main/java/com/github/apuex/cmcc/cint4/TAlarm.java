package com.github.apuex.cmcc.cint4;

import java.io.Serializable;
import java.nio.ByteBuffer;

/**
 * 当前告警值的结构
 */
public class TAlarm implements Serializable {
    private static final long serialVersionUID = 1L;

    public static void encode(ByteBuffer buf, TAlarm v) {
        Util.encodeString(buf, v.SCID, Lengths.SCID_LENGTH);
        Util.encodeString(buf, v.SerialNo, Lengths.ALARMSERIALNO_LENGTH);
        Util.encodeString(buf, v.SiteID, Lengths.SITEID_LENGTH);
        Util.encodeString(buf, v.DeviceID, Lengths.DEVICEID_LENGTH);
        Util.encodeString(buf, v.SignalID, Lengths.ID_LENGTH);
        Util.encodeString(buf, v.SignalNumber, Lengths.SIGNALNUM_LENGTH);
        Util.encodeString(buf, v.NMAlarmID, Lengths.ID_LENGTH);
        Util.encodeString(buf, v.AlarmTime, Lengths.TIME_LENGTH);
        buf.putInt(v.AlarmLevel.getValue());
        buf.putInt(v.AlarmFlag.getValue());
        buf.putFloat(v.EventValue);
        Util.encodeString(buf, v.AlarmDesc, Lengths.DES_LENGTH);
    }

    public static TAlarm decode(ByteBuffer buf) {
        TAlarm v = new TAlarm();
        v.SCID = Util.decodeString(buf, Lengths.SCID_LENGTH);
        v.SerialNo = Util.decodeString(buf, Lengths.ALARMSERIALNO_LENGTH);
        v.SiteID = Util.decodeString(buf, Lengths.SITEID_LENGTH);
        v.DeviceID = Util.decodeString(buf, Lengths.DEVICEID_LENGTH);
        v.SignalID = Util.decodeString(buf, Lengths.ID_LENGTH);
        v.SignalNumber = Util.decodeString(buf, Lengths.SIGNALNUM_LENGTH);
        v.NMAlarmID = Util.decodeString(buf, Lengths.ID_LENGTH);
        v.AlarmTime = Util.decodeString(buf, Lengths.TIME_LENGTH);
        v.AlarmLevel = EnumState.fromValue(buf.getInt());
        v.AlarmFlag = EnumFlag.fromValue(buf.getInt());
        v.EventValue = buf.getFloat();
        v.AlarmDesc = Util.decodeString(buf, Lengths.DES_LENGTH);
        return v;
    }

    @Override
    public boolean equals(Object o) {
        TAlarm r = null;
        if(o instanceof TAlarm) {
            r = (TAlarm) o;
        } else {
            return false;
        }

        boolean result =
            ( this.SCID.equals(r.SCID)
            && this.SerialNo.equals(r.SerialNo)
            && this.SiteID.equals(r.SiteID)
            && this.DeviceID.equals(r.DeviceID)
            && this.SignalID.equals(r.SignalID)
            && this.SignalNumber.equals(r.SignalNumber)
            && this.NMAlarmID.equals(r.NMAlarmID)
            && this.AlarmTime.equals(r.AlarmTime)
            && this.AlarmLevel == r.AlarmLevel
            && this.AlarmFlag == r.AlarmFlag
            && this.EventValue == r.EventValue
            && this.AlarmDesc.equals(r.AlarmDesc)
            );

        return result;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder
            .append("TAlarm { ")
            .append("SCID=").append(this.SCID)
            .append(", ").append("SerialNo=").append(this.SerialNo)
            .append(", ").append("SiteID=").append(this.SiteID)
            .append(", ").append("DeviceID=").append(this.DeviceID)
            .append(", ").append("SignalID=").append(this.SignalID)
            .append(", ").append("SignalNumber=").append(this.SignalNumber)
            .append(", ").append("NMAlarmID=").append(this.NMAlarmID)
            .append(", ").append("AlarmTime=").append(this.AlarmTime)
            .append(", ").append("AlarmLevel=").append(this.AlarmLevel)
            .append(", ").append("AlarmFlag=").append(this.AlarmFlag)
            .append(", ").append("EventValue=").append(this.EventValue)
            .append(", ").append("AlarmDesc=").append(this.AlarmDesc)
            .append(" }");

        return builder.toString();
    }

    public String SCID; // SC ID编号（7位数字，全网范围唯一，采用6位行政地区编码+1位序号组成）
    public String SerialNo; // 告警序号（10位数字，范围0~4294967295，是下级SC范围的，不足10位前面补0）
    public String SiteID; // 站点编号
    public String DeviceID; // 设备编号
    public String SignalID; // 监控点的6位信号编码，即《动环信号标准化字典表(20170927)》中的信号编码ID
    public String SignalNumber; // 同类监控点顺序号
    public String NMAlarmID; // 6位告警编码ID
    public String AlarmTime; // 告警时间，YYYY-MM-DD<SPACE键>hh:mm:ss（采用24小时的时间制式）
    public EnumState AlarmLevel; // 数据值的状态
    public EnumFlag AlarmFlag; // 告警标志
    public float EventValue; // 告警触发值
    public String AlarmDesc; // 告警描述
}

