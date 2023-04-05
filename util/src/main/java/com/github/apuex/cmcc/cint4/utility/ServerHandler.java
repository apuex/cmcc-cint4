package com.github.apuex.cmcc.cint4.utility;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.slf4j.LoggerFactory;

import com.github.apuex.cmcc.cint4.AlarmModeAck;
import com.github.apuex.cmcc.cint4.DynAccessModeAck;
import com.github.apuex.cmcc.cint4.EnumResult;
import com.github.apuex.cmcc.cint4.EnumRightLevel;
import com.github.apuex.cmcc.cint4.HeartBeat;
import com.github.apuex.cmcc.cint4.HeartBeatAck;
import com.github.apuex.cmcc.cint4.LoginAck;
import com.github.apuex.cmcc.cint4.LogoutAck;
import com.github.apuex.cmcc.cint4.Message;
import com.github.apuex.cmcc.cint4.ModifyPA;
import com.github.apuex.cmcc.cint4.ModifyPAAck;
import com.github.apuex.cmcc.cint4.SetAlarmMode;
import com.github.apuex.cmcc.cint4.SetDynAccessMode;
import com.github.apuex.cmcc.cint4.SetPoint;
import com.github.apuex.cmcc.cint4.SetPointAck;
import com.github.apuex.cmcc.cint4.TA;
import com.github.apuex.cmcc.cint4.TATD;
import com.github.apuex.cmcc.cint4.TATDArray;
import com.github.apuex.cmcc.cint4.TD;
import com.github.apuex.cmcc.cint4.TID;
import com.github.apuex.cmcc.cint4.TIDArray;
import com.github.apuex.cmcc.cint4.TimeCheckAck;
import com.github.apuex.cmcc.cint4.Util;

import ch.qos.logback.classic.Logger;
import io.netty.channel.ChannelHandlerContext;
import io.netty.util.concurrent.ScheduledFuture;

public class ServerHandler extends io.netty.channel.ChannelInboundHandlerAdapter {
	private static final Logger logger = (Logger) LoggerFactory.getLogger(ServerHandler.class);
	@SuppressWarnings("rawtypes")
	ScheduledFuture heartBeatTask;

	public ServerHandler(Map<String, String> params) {
		this.params = params;
	}
	
	@Override
	public void channelActive(ChannelHandlerContext ctx) throws Exception {
		logger.info(String.format("[%s] SYN : connection established.", ctx.channel().remoteAddress()));
		SerialNo.initSerialNo(ctx.channel());
		heartBeatTask = ctx.executor().scheduleWithFixedDelay(() -> {
			HeartBeat msg = new HeartBeat(SerialNo.nextSerialNo(ctx.channel()));
			ctx.writeAndFlush(msg);
			logger.info(String.format("[%s] SND : %s", ctx.channel().remoteAddress(), msg));
		}, 5, 5, TimeUnit.SECONDS);

		ctx.fireChannelActive();
	}

	@Override
	public void channelRead(ChannelHandlerContext ctx, Object msg) throws Exception {
		logger.info(String.format("[%s] RCV : %s", ctx.channel().remoteAddress(), msg));
		if (msg instanceof Message) {
			Message message = (Message) msg;
			switch (message.PKType) {
			case LOGIN:
				send(ctx, new LoginAck(message.SerialNo, EnumRightLevel.LEVEL2));
				break;
			case LOGIN_ACK:
				break;
			case LOGOUT:
				send(ctx, new LogoutAck(message.SerialNo));
				break;
			case LOGOUT_ACK:
				break;
			case SET_DYN_ACCESS_MODE: {
				SetDynAccessMode req = (SetDynAccessMode) message;
				List<TATD> vl = new LinkedList<TATD>();
				List<TID> idl = new LinkedList<TID>();
				TATDArray Values1 = new TATDArray(vl);
				TIDArray Values2 = new TIDArray(idl);
				send(ctx, new DynAccessModeAck(message.SerialNo, req.TerminalID, req.GroupID, EnumResult.SUCCESS, Values1, Values2));
			}
				break;
			case DYN_ACCESS_MODE_ACK:
				break;
			case SET_ALARM_MODE:
				send(ctx, new AlarmModeAck(message.SerialNo, ((SetAlarmMode) message).GroupID, EnumResult.SUCCESS));
				break;
			case ALARM_MODE_ACK:
				break;
			case SEND_ALARM:
				break;
			case SEND_ALARM_ACK:
				break;
			case SET_POINT: {
				SetPoint req = (SetPoint) message;
				if(req.Value instanceof TA) {
					TA ta = (TA)req.Value;
					send(ctx, new SetPointAck(message.SerialNo, Util.tidFrom(ta), EnumResult.SUCCESS));
				} else if(req.Value instanceof TD) {
					TD td = (TD)req.Value;
					send(ctx, new SetPointAck(message.SerialNo, Util.tidFrom(td), EnumResult.SUCCESS));
				} else {
					
				}
			}
				break;
			case SET_POINT_ACK:
				break;
			case MODIFY_PA: {
				ModifyPA req = (ModifyPA) message;
				if(req.OldPassWord != null
						&& req.OldPassWord.equals(params.get("server-passwd"))
						&& req.NewPassWord != null) {
					params.put("server-passwd", req.NewPassWord);
				}
				send(ctx, new ModifyPAAck(message.SerialNo, EnumResult.SUCCESS));
			}
				break;
			case MODIFY_PA_ACK:
				break;
			case HEART_BEAT:
				send(ctx, new HeartBeatAck(message.SerialNo));
				break;
			case HEART_BEAT_ACK:
				break;
			case TIME_CHECK:
				send(ctx, new TimeCheckAck(message.SerialNo, EnumResult.SUCCESS));
				break;
			case TIME_CHECK_ACK:
				break;
			default: // Unsupported PKType
				ctx.close();
				break;
			}
		}
		ctx.fireChannelRead(msg);
	}

	@Override
	public void channelInactive(ChannelHandlerContext ctx) throws Exception {
		logger.info(String.format("[%s] RST : connection closed.", ctx.channel().remoteAddress()));
		heartBeatTask.cancel(false);
		ctx.fireChannelInactive();
	}

	@Override
	public void exceptionCaught(ChannelHandlerContext ctx, Throwable cause) throws Exception {
		logger.info(String.format("[%s] ERR : %s", ctx.channel().remoteAddress(), cause));
		ctx.fireExceptionCaught(cause);
	}

	private void send(ChannelHandlerContext ctx, Message out) {
		ctx.writeAndFlush(out);
	}
	
	private Map<String, String> params;
}
